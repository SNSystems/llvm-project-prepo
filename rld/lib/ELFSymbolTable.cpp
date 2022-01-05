//===- lib/ELFSymbolTable.cpp ---------------------------------------------===//
//*  _____ _     _____ ____                  _           _  *
//* | ____| |   |  ___/ ___| _   _ _ __ ___ | |__   ___ | | *
//* |  _| | |   | |_  \___ \| | | | '_ ` _ \| '_ \ / _ \| | *
//* | |___| |___|  _|  ___) | |_| | | | | | | |_) | (_) | | *
//* |_____|_____|_|   |____/ \__, |_| |_| |_|_.__/ \___/|_| *
//*                          |___/                          *
//*  _____     _     _       *
//* |_   _|_ _| |__ | | ___  *
//*   | |/ _` | '_ \| |/ _ \ *
//*   | | (_| | |_) | |  __/ *
//*   |_|\__,_|_.__/|_|\___| *
//*                          *
//===----------------------------------------------------------------------===//
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

using namespace rld;

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
static constexpr unsigned char sectionToSymbolType(const SectionKind K) {
  using namespace rld;
  switch (K) {
  case SectionKind::text:
    return llvm::ELF::STT_FUNC;
  case SectionKind::bss:
  case SectionKind::data:
  case SectionKind::fini_array:
  case SectionKind::init_array:
  case SectionKind::mergeable_1_byte_c_string:
  case SectionKind::mergeable_2_byte_c_string:
  case SectionKind::mergeable_4_byte_c_string:
  case SectionKind::mergeable_const_16:
  case SectionKind::mergeable_const_32:
  case SectionKind::mergeable_const_4:
  case SectionKind::mergeable_const_8:
  case SectionKind::read_only:
  case SectionKind::rel_ro:
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
  case SectionKind::got:
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
uint64_t elf::prepareSymbolTableSection(Layout *const Lout,
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

template uint64_t elf::prepareSymbolTableSection<llvm::object::ELF32BE>(
    Layout *const Lout, const GlobalSymbolsContainer &Globals);
template uint64_t elf::prepareSymbolTableSection<llvm::object::ELF32LE>(
    Layout *const Lout, const GlobalSymbolsContainer &Globals);
template uint64_t elf::prepareSymbolTableSection<llvm::object::ELF64BE>(
    Layout *const Lout, const GlobalSymbolsContainer &Globals);
template uint64_t elf::prepareSymbolTableSection<llvm::object::ELF64LE>(
    Layout *const Lout, const GlobalSymbolsContainer &Globals);

namespace {

template <typename ELFT> struct Writer {
  using Elf_Word = typename llvm::object::ELFFile<ELFT>::Elf_Word;
  using Elf_Sym = typename llvm::object::ELFFile<ELFT>::Elf_Sym;

  // Build a symbol.
  static typename Elf_Word::value_type
  writeSymbol(Elf_Sym *const SymbolOut, const Symbol &Sym,
              typename Elf_Word::value_type NameOffset,
              const SectionIndexedArray<unsigned> &SectionToIndex) {
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
      const pstore::repo::section_kind Kind = B.fragment()->front();

      SymbolOut->st_value = elf::symbolValue(*C);
      SymbolOut->st_shndx = SectionToIndex[C->OScn->SectionK];
      SymbolOut->st_size = std::accumulate(
          Bodies->begin(), Bodies->end(), uint64_t{0},
          [&Kind](const uint64_t Acc, Symbol::Body const &Body) -> uint64_t {
            return Acc + fragmentSectionSize(Kind, Body.fragment());
          });
      SymbolOut->setVisibility(elf::elfVisibility<ELFT>(B.visibility()));
      SymbolOut->setBindingAndType(linkageToELFBinding(B.linkage()),
                                   sectionToSymbolType(C->OScn->SectionK));
    }
    SymbolOut->st_name = Elf_Word{NameOffset};

    // Pass our lock to nameLength() so that it doesn't try to take one of its
    // own. The +1 here allows for the final '\0'.
    NameOffset += Sym.nameLength() + 1U;
    return NameOffset;
  }

  static constexpr uint64_t
  fragmentSectionSize(const pstore::repo::section_kind Kind,
                      FragmentPtr const &F) {
    if (!F->has_section(Kind)) {
      return 0U;
    }
#define X(x)                                                                   \
  case pstore::repo::section_kind::x:                                          \
    return F->at<pstore::repo::section_kind::x>().size();

    switch (Kind) {
      PSTORE_MCREPO_SECTION_KINDS
    case pstore::repo::section_kind::last:
      break;
    }
#undef X
    return 0U;
  }
};

} // end anonymous namespace

// write symbol table
// ~~~~~~~~~~~~~~~~~~
template <typename ELFT>
typename llvm::object::ELFFile<ELFT>::Elf_Sym *
elf::writeSymbolTable(typename llvm::object::ELFFile<ELFT>::Elf_Sym *SymbolOut,
                      const SymbolOrder &SymOrder,
                      const UndefsContainer &Undefs,
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
    NameOffset =
        Writer<ELFT>::writeSymbol(SymbolOut, Sym, NameOffset, SectionToIndex);
    ++SymbolOut;
  });
  return SymbolOut;
}

template auto elf::writeSymbolTable<llvm::object::ELF32BE>(
    typename llvm::object::ELFFile<llvm::object::ELF32BE>::Elf_Sym *SymbolOut,
    const SymbolOrder &SymOrder, const UndefsContainer &Undefs,
    const SectionIndexedArray<unsigned> &SectionToIndex) ->
    typename llvm::object::ELFFile<llvm::object::ELF32BE>::Elf_Sym *;

template auto elf::writeSymbolTable<llvm::object::ELF32LE>(
    typename llvm::object::ELFFile<llvm::object::ELF32LE>::Elf_Sym *SymbolOut,
    const SymbolOrder &SymOrder, const UndefsContainer &Undefs,
    const SectionIndexedArray<unsigned> &SectionToIndex) ->
    typename llvm::object::ELFFile<llvm::object::ELF32LE>::Elf_Sym *;

template auto elf::writeSymbolTable<llvm::object::ELF64BE>(
    typename llvm::object::ELFFile<llvm::object::ELF64BE>::Elf_Sym *SymbolOut,
    const SymbolOrder &SymOrder, const UndefsContainer &Undefs,
    const SectionIndexedArray<unsigned> &SectionToIndex) ->
    typename llvm::object::ELFFile<llvm::object::ELF64BE>::Elf_Sym *;

template auto elf::writeSymbolTable<llvm::object::ELF64LE>(
    typename llvm::object::ELFFile<llvm::object::ELF64LE>::Elf_Sym *SymbolOut,
    const SymbolOrder &SymOrder, const UndefsContainer &Undefs,
    const SectionIndexedArray<unsigned> &SectionToIndex) ->
    typename llvm::object::ELFFile<llvm::object::ELF64LE>::Elf_Sym *;
