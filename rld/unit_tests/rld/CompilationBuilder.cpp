//*   ____                      _ _       _   _             ____        _ _     _  *
//*  / ___|___  _ __ ___  _ __ (_) | __ _| |_(_) ___  _ __ | __ ) _   _(_) | __| | *
//* | |   / _ \| '_ ` _ \| '_ \| | |/ _` | __| |/ _ \| '_ \|  _ \| | | | | |/ _` | *
//* | |__| (_) | | | | | | |_) | | | (_| | |_| | (_) | | | | |_) | |_| | | | (_| | *
//*  \____\___/|_| |_| |_| .__/|_|_|\__,_|\__|_|\___/|_| |_|____/ \__,_|_|_|\__,_| *
//*                      |_|                                                       *
//*             *
//*   ___ _ __  *
//*  / _ \ '__| *
//* |  __/ |    *
//*  \___|_|    *
//*             *
//===- unit_tests/rld/CompilationBuilder.cpp ------------------------------===//
// Copyright (c) 2017-2020 by Sony Interactive Entertainment, Inc.
// All rights reserved.
//
// Developed by:
//   Toolchain Team
//   SN Systems, Ltd.
//   www.snsystems.com
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal with the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject to
// the following conditions:
//
// - Redistributions of source code must retain the above copyright notice,
//   this list of conditions and the following disclaimers.
//
// - Redistributions in binary form must reproduce the above copyright
//   notice, this list of conditions and the following disclaimers in the
//   documentation and/or other materials provided with the distribution.
//
// - Neither the names of SN Systems Ltd., Sony Interactive Entertainment,
//   Inc. nor the names of its contributors may be used to endorse or
//   promote products derived from this Software without specific prior
//   written permission.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
// IN NO EVENT SHALL THE CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR
// ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
// TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
// SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE SOFTWARE.
//===----------------------------------------------------------------------===//
#include "CompilationBuilder.h"

#include "pstore/core/hamt_map.hpp"
#include "pstore/core/hamt_set.hpp"

// compile
// ~~~~~~~
std::shared_ptr<pstore::repo::compilation const>
CompilationBuilder::compile(std::string const &DefinitionName,
                            pstore::repo::linkage Linkage) {
  assert(Linkage != pstore::repo::linkage::common);
  std::array<NameAndLinkagePair, 1> NameAndLinkage{{{DefinitionName, Linkage}}};
  return compile(std::begin(NameAndLinkage), std::end(NameAndLinkage),
                 createFragmentWithReadOnlySection);
}

// compile with common symbol
// ~~~~~~~~~~~~~~~~~~~~~~~~~~
std::shared_ptr<pstore::repo::compilation const>
CompilationBuilder::compileWithCommonSymbol(std::string const &DefinitionName,
                                            unsigned CommonSize) {
  using namespace pstore::repo;

  std::array<NameAndLinkagePair, 1> NameAndLinkage{
      {{DefinitionName, linkage::common}}};
  return compile(std::begin(NameAndLinkage), std::end(NameAndLinkage),
                 [CommonSize](auto &Transaction, StringAdder &) {
                   return createFragmentWithBSSSection(Transaction, CommonSize);
                 });
}

// create fragment with read-only section [static]
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pstore::index::fragment_index::value_type
CompilationBuilder::createFragmentWithReadOnlySection(
    Transaction &T, StringAdder & /*NameAdder*/) {
  using pstore::repo::fragment;
  using pstore::repo::generic_section_creation_dispatcher;
  using pstore::repo::section_content;
  using pstore::repo::section_kind;

  auto FragmentIndex = getFragmentIndex(T.db());
  auto const NumFragments = FragmentIndex->size();

  section_content DataSection{section_kind::read_only,
                              uint8_t{1} /*alignment*/};
  assert(NumFragments <
         std::numeric_limits<decltype(DataSection.data)::value_type>::max());
  DataSection.data.push_back(NumFragments);

  std::array<generic_section_creation_dispatcher, 1> Dispatchers{
      {{section_kind::read_only, &DataSection}}};
  return *FragmentIndex
              ->insert(
                  T, std::make_pair(pstore::index::digest{NumFragments},
                                    fragment::alloc(T, std::begin(Dispatchers),
                                                    std::end(Dispatchers))))
              .first;
}

// create fragment with BSS section [static]
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pstore::index::fragment_index::value_type
CompilationBuilder::createFragmentWithBSSSection(Transaction &T, size_t Size) {
  using pstore::repo::bss_section_creation_dispatcher;
  using pstore::repo::fragment;
  using pstore::repo::section_content;
  using pstore::repo::section_kind;

  auto FragmentIndex = getFragmentIndex(T.db());
  auto const NumFragments = FragmentIndex->size();

  section_content BSSSection{section_kind::bss, uint8_t{1} /*alignment*/};
  BSSSection.data.resize(Size);
  bss_section_creation_dispatcher CD{&BSSSection};
  return *FragmentIndex
              ->insert(T, std::make_pair(pstore::index::digest{NumFragments},
                                         fragment::alloc(T, &CD, &CD + 1)))
              .first;
};

// store string
// ~~~~~~~~~~~~
rld::StringAddress CompilationBuilder::storeString(char const *Name) {
  auto Transaction = pstore::begin(Db_);
  rld::StringAddress const SAddr = NameAdder_.add(Transaction, Name);
  NameAdder_.flush(Transaction); // Write the string body.
  Transaction.commit();
  return SAddr;
}

// get fragment index
// ~~~~~~~~~~~~~~~~~~
std::shared_ptr<pstore::index::fragment_index>
CompilationBuilder::getFragmentIndex(pstore::database &Db) {
  return pstore::index::get_index<pstore::trailer::indices::fragment>(Db);
}

// get compilation index
// ~~~~~~~~~~~~~~~~~~~~~
std::shared_ptr<pstore::index::compilation_index>
CompilationBuilder::getCompilationIndex(pstore::database &Db) {
  return pstore::index::get_index<pstore::trailer::indices::compilation>(Db);
}

// get name index
// ~~~~~~~~~~~~~~
std::shared_ptr<pstore::index::name_index>
CompilationBuilder::getNameIndex(pstore::database &Db) {
  return pstore::index::get_index<pstore::trailer::indices::name>(Db);
}
