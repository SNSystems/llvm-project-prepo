//===- lib/ELFSymbolTable.cpp ---------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#include "rld/ELFSymbolTable.h"

#include "llvm/Object/ELFTypes.h"

#include "rld/ElfOutput.h"
#include "rld/LayoutBuilder.h"
#include "rld/SectionArray.h"
#include "rld/elf.h"

// FIXME: this function was lifted from R2OSymbolTable.h
static constexpr unsigned char
linkageToELFBinding(const pstore::repo::linkage L) {
  using namespace pstore::repo;
  switch (L) {
  case linkage::internal_no_symbol:
  case linkage::internal:
    return llvm::ELF::STB_LOCAL;
  case linkage::link_once_any:
  case linkage::link_once_odr:
  case linkage::weak_any:
  case linkage::weak_odr:
    return llvm::ELF::STB_WEAK;
  default:
    return llvm::ELF::STB_GLOBAL;
  }
}

// FIXME: there is an almost identical function in R2OSymbolTable.h
static constexpr unsigned char sectionToSymbolType(const rld::SectionKind K) {
  using namespace rld;
  switch (K) {
  case SectionKind::text:
    return llvm::ELF::STT_FUNC;
  case SectionKind::bss:
  case SectionKind::data:
  case SectionKind::rel_ro:
  case SectionKind::mergeable_1_byte_c_string:
  case SectionKind::mergeable_2_byte_c_string:
  case SectionKind::mergeable_4_byte_c_string:
  case SectionKind::mergeable_const_4:
  case SectionKind::mergeable_const_8:
  case SectionKind::mergeable_const_16:
  case SectionKind::mergeable_const_32:
  case SectionKind::read_only:
    return llvm::ELF::STT_OBJECT;
  case SectionKind::thread_bss:
  case SectionKind::thread_data:
    return llvm::ELF::STT_TLS;
  case SectionKind::debug_line:
  case SectionKind::debug_ranges:
  case SectionKind::debug_string:
    return llvm::ELF::STT_NOTYPE;
  case SectionKind::linked_definitions:
  case SectionKind::rela_plt:
  case SectionKind::gotplt:
  case SectionKind::plt:
  case SectionKind::shstrtab:
  case SectionKind::strtab:
  case SectionKind::symtab:
  case SectionKind::last:
    break;
  }
  llvm_unreachable("invalid section type");
}

// prepare symbol table section
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
template <typename ELFT>
uint64_t
rld::elf::prepareSymbolTableSection(Layout *const Lout,
                                    const GlobalSymbolsContainer &Globals) {
  using Elf_Sym = typename llvm::object::ELFFile<ELFT>::Elf_Sym;
  // TODO: this shouldn't include the symbols with internal_no_symbol linkage.
  const uint64_t NumSymbols = Globals.size() + 1;

  OutputSection &StrTab = Lout->Sections[SectionKind::symtab];
  StrTab.AlwaysEmit = true;
  StrTab.FileSize = NumSymbols * sizeof(Elf_Sym);
  StrTab.MaxAlign = alignof(Elf_Sym);
  StrTab.Link = SectionKind::strtab;
  return NumSymbols;
}

template uint64_t rld::elf::prepareSymbolTableSection<llvm::object::ELF32BE>(
    Layout *const Lout, const GlobalSymbolsContainer &Globals);
template uint64_t rld::elf::prepareSymbolTableSection<llvm::object::ELF32LE>(
    Layout *const Lout, const GlobalSymbolsContainer &Globals);
template uint64_t rld::elf::prepareSymbolTableSection<llvm::object::ELF64BE>(
    Layout *const Lout, const GlobalSymbolsContainer &Globals);
template uint64_t rld::elf::prepareSymbolTableSection<llvm::object::ELF64LE>(
    Layout *const Lout, const GlobalSymbolsContainer &Globals);

// write symbol table
// ~~~~~~~~~~~~~~~~~~
template <typename ELFT>
typename llvm::object::ELFFile<ELFT>::Elf_Sym *rld::elf::writeSymbolTable(
    typename llvm::object::ELFFile<ELFT>::Elf_Sym *SymbolOut,
    const SymbolOrder &SymOrder, const UndefsContainer &Undefs,
    const SectionIndexedArray<unsigned> &SectionToIndex) {
  using Elf_Word = typename llvm::object::ELFFile<ELFT>::Elf_Word;
  static_assert(std::is_unsigned<typename Elf_Word::value_type>::value,
                "Expected ELF_Word to be unsigned");

  auto NameOffset =
      typename Elf_Word::value_type{1}; // 1 to allow for the initial '\0'.
  // The initial null symbol.
  std::memset(SymbolOut, 0, sizeof(*SymbolOut));
  ++SymbolOut;

  SymOrder.walk(Undefs, [&](const Symbol &Sym) {
    // Build a symbol.
    const std::tuple<const Symbol::OptionalBodies &,
                     std::unique_lock<Symbol::Mutex>>
        Def = Sym.definition();
    auto &Bodies = std::get<const Symbol::OptionalBodies &>(Def);
    auto &Lock = std::get<std::unique_lock<Symbol::Mutex>>(Def);
    if (!Bodies) {
      // This is an undefined symbol.
      std::memset(SymbolOut, 0, sizeof(*SymbolOut));
      assert(Sym.allReferencesAreWeak(Lock) &&
             "Undefined entries in the symbol table must be weakly referenced");
      SymbolOut->setBinding(llvm::ELF::STB_WEAK);
    } else {
      // If there are multiple bodies associated with a symbol then the ELF
      // symbol simply points to the first.
      const Symbol::Body &B = Bodies->front();
      const Contribution *const C = Sym.contribution();
      assert(C != nullptr);

      SymbolOut->st_value = symbolValue(*C);
      SymbolOut->st_shndx = SectionToIndex[C->OScn->SectionK];
      // FIXME: the sum of the sizes of all of the bodies.
      SymbolOut->st_size = 0;
      SymbolOut->setVisibility(elfVisibility<ELFT>(B.visibility()));
      SymbolOut->setBindingAndType(linkageToELFBinding(B.linkage()),
                                   sectionToSymbolType(C->OScn->SectionK));
    }
    SymbolOut->st_name = Elf_Word{NameOffset};

    ++SymbolOut;
    // Pass our lock to nameLength() so that it doesn't try to take one of its
    // own. The +1 here allows for the final '\0'.
    NameOffset += Sym.nameLength(Lock) + 1U;
  });
  return SymbolOut;
}

template auto rld::elf::writeSymbolTable<llvm::object::ELF32BE>(
    typename llvm::object::ELFFile<llvm::object::ELF32BE>::Elf_Sym *SymbolOut,
    const SymbolOrder &SymOrder, const UndefsContainer &Undefs,
    const SectionIndexedArray<unsigned> &SectionToIndex) ->
    typename llvm::object::ELFFile<llvm::object::ELF32BE>::Elf_Sym *;

template auto rld::elf::writeSymbolTable<llvm::object::ELF32LE>(
    typename llvm::object::ELFFile<llvm::object::ELF32LE>::Elf_Sym *SymbolOut,
    const SymbolOrder &SymOrder, const UndefsContainer &Undefs,
    const SectionIndexedArray<unsigned> &SectionToIndex) ->
    typename llvm::object::ELFFile<llvm::object::ELF32LE>::Elf_Sym *;

template auto rld::elf::writeSymbolTable<llvm::object::ELF64BE>(
    typename llvm::object::ELFFile<llvm::object::ELF64BE>::Elf_Sym *SymbolOut,
    const SymbolOrder &SymOrder, const UndefsContainer &Undefs,
    const SectionIndexedArray<unsigned> &SectionToIndex) ->
    typename llvm::object::ELFFile<llvm::object::ELF64BE>::Elf_Sym *;

template auto rld::elf::writeSymbolTable<llvm::object::ELF64LE>(
    typename llvm::object::ELFFile<llvm::object::ELF64LE>::Elf_Sym *SymbolOut,
    const SymbolOrder &SymOrder, const UndefsContainer &Undefs,
    const SectionIndexedArray<unsigned> &SectionToIndex) ->
    typename llvm::object::ELFFile<llvm::object::ELF64LE>::Elf_Sym *;
