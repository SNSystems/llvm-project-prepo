//===- lib/ELFStringTable.cpp ---------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#include "rld/ELFStringTable.h"

#include "rld/LayoutBuilder.h"

// prepare string table
// ~~~~~~~~~~~~~~~~~~~~
uint64_t rld::elf::prepareStringTable(Layout *const Lout, const Context &Ctxt,
                                      const GlobalSymbolsContainer &Globals) {
  const auto StringTableSize = Ctxt.ELFStringTableSize.load();
  (void)Globals;
  assert(StringTableSize ==
         std::accumulate(std::begin(Globals), std::end(Globals), uint64_t{1},
                         [&Ctxt](const uint64_t Acc, const Symbol &Sym) {
                           const auto Length =
                               stringLength(Ctxt.Db, Sym.name());
                           assert(Length == Sym.nameLength());
                           return Acc + Length + 1U;
                         }));

  OutputSection &StrTab = Lout->Sections[SectionKind::strtab];
  StrTab.AlwaysEmit = true;
  StrTab.FileSize = StringTableSize;
  StrTab.MaxAlign = 1U;
  return StringTableSize;
}

// write strings
// ~~~~~~~~~~~~~
uint8_t *rld::elf::writeStrings(uint8_t *StringOut, const Context &Context,
                                const SymbolOrder &SymOrder,
                                const UndefsContainer &Undefs) {
  *(StringOut++) = '\0'; // The string table's initial null entry.
  pstore::shared_sstring_view Owner;

  SymOrder.walk(Undefs, [&](const Symbol &Sym) {
    // Copy a string.
    const pstore::raw_sstring_view Str = pstore::get_sstring_view(
        Context.Db, Sym.name(), Sym.nameLength(), &Owner);
    auto *const StringOutEnd =
        std::copy(std::begin(Str), std::end(Str), StringOut);
    assert(StringOut + Sym.nameLength() == StringOutEnd);
    StringOut = StringOutEnd;
    *(StringOut++) = '\0';
  });

  return StringOut;
}
