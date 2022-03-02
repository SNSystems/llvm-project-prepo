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

#include "rld/ELF.h"
#include "rld/ELFOutput.h"
#include "rld/LayoutBuilder.h"
#include "rld/SectionArray.h"

#include "rld/Copy.h" // for WorkItem

#include "llvm/Object/ELFTypes.h"

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
  case SectionKind::debug_loc:
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

// symbol size
// ~~~~~~~~~~~
static uint64_t symbolSize(const pstore::repo::section_kind Kind,
                           const Symbol::BodyContainer &Bodies) {
  return std::accumulate(Bodies.begin(), Bodies.end(), uint64_t{0},
                         [Kind](const uint64_t Acc, Symbol::Body const &Body) {
                           const FragmentPtr &F = Body.fragment();
                           if (!F->has_section(Kind)) {
                             return Acc;
                           }
                           auto Size = uint64_t{0};
#define X(x)                                                                   \
  case pstore::repo::section_kind::x:                                          \
    Size = F->at<pstore::repo::section_kind::x>().size();                      \
    break;

                           switch (Kind) {
                             PSTORE_MCREPO_SECTION_KINDS
                           case pstore::repo::section_kind::last:
                             break;
                           }
#undef X
                           return Acc + Size;
                         });
}

namespace {

template <typename ELFT> class Writer {
public:
  using Elf_Sym = typename llvm::object::ELFFile<ELFT>::Elf_Sym;

  /// Adds the work of writing the collection of symbols given by \p EmitList to
  /// the job work \p Q. The start output address is given by \p Dest.
  ///
  /// \param Q  The queue to which work is added.
  /// \param Dest  The address of the symbol table storage in the output file.
  /// \param EmitList  The list of symbols to be emitted.
  /// \returns  The end of the range of address that will be written by the
  ///   queued work.
  static Elf_Sym *schedulePartitions(MPMCQueue<WorkItem> &Q, Elf_Sym *Dest,
                                     const SymbolEmitList &EmitList);

  /// Adds the work of writing the undefined symbols given by \p Undefs to the
  /// job work \p Q. The start output address is given by \p Dest.
  ///
  /// \param Q  The queue to which work is added.
  /// \param Dest  The address of the symbol table storage in the output file.
  /// \param Undefs  The collection of undefined symbols.
  /// \returns  The end of the range of address that will be written by the
  ///   queued work.
  static Elf_Sym *scheduleUndefRange(MPMCQueue<WorkItem> &Q, Elf_Sym *Dest,
                                     const UndefsContainer &Undefs);

private:
  static void
  writeSymbolTablePartition(uint8_t *Dest, Context &Context, const Layout &Lout,
                            const GOTPLTContainer &GOTPLTs,
                            const SectionIndexedArray<unsigned> &SectionToIndex,
                            WorkItem::Pointer First, WorkItem::Pointer Last);

  static void
  writeUndefSymbolRange(uint8_t *Dest, Context &Context, const Layout &Lout,
                        const GOTPLTContainer &GOTPLTs,
                        const SectionIndexedArray<unsigned> &SectionToIndex,
                        WorkItem::Pointer First, WorkItem::Pointer Last);

  /// Write an individual ELF symbol.
  ///
  /// \param SymbolOut  The memory to which the symbol table entry will be
  /// written.
  /// \param Sym  The symbol to be written.
  /// \param SectionToIndex  The mapping of rld to ELF sections.
  static Elf_Sym *
  writeSymbol(Elf_Sym *const SymbolOut, const Symbol &Sym,
              const SectionIndexedArray<unsigned> &SectionToIndex);
};

// Symbol Table Partitions
// ~~~~~~~~~~~~~~~~~~~~~~~
template <typename ELFT>
auto Writer<ELFT>::schedulePartitions(MPMCQueue<WorkItem> &Q, Elf_Sym *Dest,
                                      const SymbolEmitList &EmitList)
    -> Elf_Sym * {
  auto Out = Dest;
  for (auto Pos = EmitList.partitionsBegin(); *Pos != nullptr;) {
    auto Next = Pos;
    std::advance(Next, 1);
    Q.emplace(reinterpret_cast<uint8_t *>(Out), &writeSymbolTablePartition,
              WorkItem::Pointer{in_place_type_t<const Symbol *>{}, *Pos},
              WorkItem::Pointer{in_place_type_t<const Symbol *>{}, *Next});
    Out += SymbolEmitList::PartitionSize;
    Pos = Next;
  }
  // The last partition may not be full, so compute the last symbol address
  // here.
  return Dest + EmitList.size();
}

template <typename ELFT>
void Writer<ELFT>::writeSymbolTablePartition(
    uint8_t *Dest, Context & /*Context*/, const Layout & /*Lout*/,
    const GOTPLTContainer & /*GOTPLTs*/,
    const SectionIndexedArray<unsigned> &SectionToIndex,
    WorkItem::Pointer First, WorkItem::Pointer Last) {

  auto *SymbolOut = reinterpret_cast<Elf_Sym *>(Dest);
  auto *const End = get<Symbol const *>(Last);
  for (auto *Sym = get<Symbol const *>(First); Sym != End;
       Sym = Sym->NextEmit) {
    assert(Sym != nullptr);
    SymbolOut = Writer<ELFT>::writeSymbol(SymbolOut, *Sym, SectionToIndex);
  }
}

// Undefined Symbols
// ~~~~~~~~~~~~~~~~~
template <typename ELFT>
auto Writer<ELFT>::scheduleUndefRange(MPMCQueue<WorkItem> &Q, Elf_Sym *Dest,
                                      const UndefsContainer &Undefs)
    -> Elf_Sym * {
  Q.emplace(reinterpret_cast<uint8_t *>(Dest), &writeUndefSymbolRange,
            WorkItem::Pointer{Undefs.begin()}, WorkItem::Pointer{Undefs.end()});
  return Dest + std::distance(Undefs.begin(), Undefs.end());
}

template <typename ELFT>
void Writer<ELFT>::writeUndefSymbolRange(
    uint8_t *Dest, Context & /*Context*/, const Layout & /*Lout*/,
    const GOTPLTContainer & /*GOTPLTs*/,
    const SectionIndexedArray<unsigned> &SectionToIndex,
    WorkItem::Pointer First, WorkItem::Pointer Last) {

  using Iterator = UndefsContainer::Container::const_iterator;
  auto *SymbolOut = reinterpret_cast<Elf_Sym *>(Dest);
  std::for_each(
      get<Iterator>(First), get<Iterator>(Last), [&](const Symbol &Sym) {
        SymbolOut = Writer<ELFT>::writeSymbol(SymbolOut, Sym, SectionToIndex);
      });
}

// Build a symbol.
template <typename ELFT>
auto Writer<ELFT>::writeSymbol(
    Elf_Sym *const SymbolOut, const Symbol &Sym,
    const SectionIndexedArray<unsigned> &SectionToIndex) -> Elf_Sym * {
  const auto ContentsAndLock = Sym.contentsAndLock();
  const auto &Contents = std::get<const Symbol::Contents &>(ContentsAndLock);
  auto &Lock = std::get<std::unique_lock<Symbol::Mutex>>(ContentsAndLock);
  (void)Lock;
  if (holdsAlternative<Symbol::Reference>(Contents)) {
    // This is an undefined symbol.
    std::memset(SymbolOut, 0, sizeof(*SymbolOut));
    assert(Sym.allReferencesAreWeak(Lock) &&
           "Undefined entries in the symbol table must be weakly referenced");
    SymbolOut->st_name = Sym.elfNameOffset();
    SymbolOut->setBinding(llvm::ELF::STB_WEAK);
    return SymbolOut + 1;
  }

  // If there are multiple bodies associated with a symbol then the ELF
  // symbol simply points to the first.
  const auto &D = get<Symbol::BodyContainer>(Contents);
  const auto &FirstBody = D.front();

  const Contribution *const C = Sym.contribution();
  assert(C != nullptr);
  SymbolOut->st_name = Sym.elfNameOffset();
  SymbolOut->setBindingAndType(linkageToELFBinding(FirstBody.linkage()),
                               sectionToSymbolType(C->OScn->SectionK));
  SymbolOut->setVisibility(elf::elfVisibility<ELFT>(FirstBody.visibility()));
  SymbolOut->st_shndx = SectionToIndex[C->OScn->SectionK];
  SymbolOut->st_value = elf::symbolValue(*C);
  SymbolOut->st_size = symbolSize(FirstBody.fragment()->front(), D);
  return SymbolOut + 1;
}

} // end anonymous namespace

// schedule symbol table
// ~~~~~~~~~~~~~~~~~~~~~
template <typename ELFT>
void rld::elf::scheduleSymbolTable(
    Context &Context, MPMCQueue<WorkItem> &Q, uint8_t *const Out,
    const SymbolOrder &SymOrder, const UndefsContainer &Undefs,
    const SectionIndexedArray<unsigned> &SectionToIndex,
    const uint64_t SymbolTableSize) {
  using Elf_Sym = typename llvm::object::ELFFile<ELFT>::Elf_Sym;
  auto *SymbolOut = reinterpret_cast<Elf_Sym *>(Out);

  // The initial null symbol. Easiest to get that special case out of the way as
  // soon as possible.
  std::memset(SymbolOut, 0, sizeof(Elf_Sym));
  ++SymbolOut;
  // Locals and globals
  SymbolOut = Writer<ELFT>::schedulePartitions(Q, SymbolOut, SymOrder.locals());
  SymbolOut =
      Writer<ELFT>::schedulePartitions(Q, SymbolOut, SymOrder.globals());
  // Undefined symbols.
  SymbolOut = Writer<ELFT>::scheduleUndefRange(Q, SymbolOut, Undefs);

  // FIXME: check against SymbolTableSize. Bytes or values?
  //  assert (  auto *SymbolOut = reinterpret_cast<Elf_Sym *>(Out);
}

template void rld::elf::scheduleSymbolTable<llvm::object::ELF32BE>(
    Context &Context, MPMCQueue<WorkItem> &Q, uint8_t *const Out,
    const SymbolOrder &SymOrder, const UndefsContainer &Undefs,
    const SectionIndexedArray<unsigned> &SectionToIndex,
    const uint64_t SymbolTableSize);
template void rld::elf::scheduleSymbolTable<llvm::object::ELF32LE>(
    Context &Context, MPMCQueue<WorkItem> &Q, uint8_t *const Out,
    const SymbolOrder &SymOrder, const UndefsContainer &Undefs,
    const SectionIndexedArray<unsigned> &SectionToIndex,
    const uint64_t SymbolTableSize);
template void rld::elf::scheduleSymbolTable<llvm::object::ELF64BE>(
    Context &Context, MPMCQueue<WorkItem> &Q, uint8_t *const Out,
    const SymbolOrder &SymOrder, const UndefsContainer &Undefs,
    const SectionIndexedArray<unsigned> &SectionToIndex,
    const uint64_t SymbolTableSize);
template void rld::elf::scheduleSymbolTable<llvm::object::ELF64LE>(
    Context &Context, MPMCQueue<WorkItem> &Q, uint8_t *const Out,
    const SymbolOrder &SymOrder, const UndefsContainer &Undefs,
    const SectionIndexedArray<unsigned> &SectionToIndex,
    const uint64_t SymbolTableSize);
