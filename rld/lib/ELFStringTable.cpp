//===- lib/ELFStringTable.cpp ---------------------------------------------===//
//*  _____ _     _____ ____  _        _               _____     _     _       *
//* | ____| |   |  ___/ ___|| |_ _ __(_)_ __   __ _  |_   _|_ _| |__ | | ___  *
//* |  _| | |   | |_  \___ \| __| '__| | '_ \ / _` |   | |/ _` | '_ \| |/ _ \ *
//* | |___| |___|  _|  ___) | |_| |  | | | | | (_| |   | | (_| | |_) | |  __/ *
//* |_____|_____|_|   |____/ \__|_|  |_|_| |_|\__, |   |_|\__,_|_.__/|_|\___| *
//*                                           |___/                           *
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#include "rld/ELFStringTable.h"

// prepare string table section
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
uint64_t
rld::elf::prepareStringTableSection(Layout *const Layout,
                                    const Context &Context,
                                    const GlobalSymbolsContainer &Globals) {
  const auto StringTableSize = Context.ELFStringTableSize.load();
  (void)Globals;
  assert(StringTableSize ==
         std::accumulate(std::begin(Globals), std::end(Globals), uint64_t{1},
                         [&Context](const uint64_t Acc, const Symbol &Sym) {
                           const auto Length =
                               stringLength(Context.Db, Sym.name());
                           assert(Length == Sym.nameLength());
                           return Acc + Length + 1U;
                         }));

  OutputSection &StrTab = Layout->Sections[SectionKind::strtab];
  StrTab.AlwaysEmit = true;
  StrTab.FileSize = StringTableSize;
  StrTab.MaxAlign = 1U;
  return StringTableSize;
}

using namespace rld;

// Copy a string.
static uint8_t *writeString(uint8_t *Dest, Context &Context,
                            const Symbol &Sym) {
  pstore::shared_sstring_view Owner;
  const pstore::raw_sstring_view Str = pstore::get_sstring_view(
      Context.Db, Sym.name(), Sym.nameLength(), &Owner);
  auto *const StringOutEnd = std::copy(std::begin(Str), std::end(Str), Dest);
  assert(Dest + Sym.nameLength() == StringOutEnd);
  Dest = StringOutEnd;
  *(Dest++) = '\0';
  return Dest;
}

// Symbol Table Partition Names
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
static void
writeStringTablePartition(uint8_t *Dest, Context &Context,
                          const Layout & /*Lout*/,
                          const GOTPLTContainer & /*GOTPLTs*/,
                          const SectionIndexedArray<unsigned> &SectionToIndex,
                          WorkItem::Pointer First, WorkItem::Pointer Last) {

  auto *const End = get<Symbol const *>(Last);
  for (auto *Sym = get<Symbol const *>(First); Sym != End;
       Sym = Sym->NextEmit) {
    assert(Sym != nullptr);
    writeString(Dest + Sym->elfNameOffset(), Context, *Sym);
  }
}

static void schedulePartitions(MPMCQueue<WorkItem> &Q, uint8_t *Dest,
                               const SymbolEmitList &EmitList) {
  for (auto Pos = EmitList.partitionsBegin(); *Pos != nullptr;) {
    auto Next = Pos;
    std::advance(Next, 1);
    Q.emplace(Dest, &writeStringTablePartition,
              WorkItem::Pointer{in_place_type_t<const Symbol *>{}, *Pos},
              WorkItem::Pointer{in_place_type_t<const Symbol *>{}, *Next});
    Pos = Next;
  }
}

// Undefined Symbol Names
// ~~~~~~~~~~~~~~~~~~~~~~
static void
writeUndefSymbolRange(uint8_t *Dest, Context &Context, const Layout & /*Lout*/,
                      const GOTPLTContainer & /*GOTPLTs*/,
                      const SectionIndexedArray<unsigned> & /*SectionToIndex*/,
                      WorkItem::Pointer First, WorkItem::Pointer Last) {

  using Iterator = UndefsContainer::Container::const_iterator;
  std::for_each(get<Iterator>(First), get<Iterator>(Last),
                [Dest, &Context](const Symbol &Sym) {
                  writeString(Dest + Sym.elfNameOffset(), Context, Sym);
                });
}

static void scheduleUndefRange(MPMCQueue<WorkItem> &Q, uint8_t *Dest,
                               const UndefsContainer &Undefs) {
  Q.emplace(Dest, &writeUndefSymbolRange, WorkItem::Pointer{Undefs.begin()},
            WorkItem::Pointer{Undefs.end()});
}

template <typename ELFT>
void rld::elf::scheduleStrings(Context &Context, MPMCQueue<WorkItem> &Q,
                               uint8_t *Out, const SymbolOrder &SymOrder,
                               const UndefsContainer &Undefs) {

  *Out = '\0'; // The string table's initial null entry.
  // Locals and globals
  schedulePartitions(Q, Out, SymOrder.locals());
  schedulePartitions(Q, Out, SymOrder.globals());
  // Undefined symbols.
  scheduleUndefRange(Q, Out, Undefs);
}

template void rld::elf::scheduleStrings<llvm::object::ELF32BE>(
    Context &Context, MPMCQueue<WorkItem> &Q, uint8_t *Out,
    const SymbolOrder &SymOrder, const UndefsContainer &Undefs);
template void rld::elf::scheduleStrings<llvm::object::ELF32LE>(
    Context &Context, MPMCQueue<WorkItem> &Q, uint8_t *Out,
    const SymbolOrder &SymOrder, const UndefsContainer &Undefs);
template void rld::elf::scheduleStrings<llvm::object::ELF64BE>(
    Context &Context, MPMCQueue<WorkItem> &Q, uint8_t *Out,
    const SymbolOrder &SymOrder, const UndefsContainer &Undefs);
template void rld::elf::scheduleStrings<llvm::object::ELF64LE>(
    Context &Context, MPMCQueue<WorkItem> &Q, uint8_t *Out,
    const SymbolOrder &SymOrder, const UndefsContainer &Undefs);
