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
//===- unit_tests/rld/CompilationBuilder.h --------------------------------===//
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
#ifndef RLD_UNIT_TESTS_COMPILATION_BUILDER_H
#define RLD_UNIT_TESTS_COMPILATION_BUILDER_H

#include "StringAdder.h"

class CompilationBuilder {
public:
  using NameAndLinkagePair = std::pair<std::string, pstore::repo::linkage>;
  using Transaction = pstore::transaction<pstore::transaction_lock>;

  explicit CompilationBuilder(pstore::database &Db) : Db_{Db}, NameAdder_{Db} {}

  /// \tparam NameAndLinkageIterator An iterator whose value-type must be
  ///     NameAndLinkagePair.
  /// \tparam CreateFragmentFn  A function which is called to create a single
  ///     fragment. It should be compatible with the signature:
  ///     function<pair<digest, extent<fragment>>(Transaction&)>
  /// \param First  The beginning of an iterator range which produces
  ///     NameAndLinkagePair instances.
  /// \param Last  The end of an iterator range which produces
  ///     NameAndLinkagePair instances.
  /// \param FragmentCreator  A function which will create a single fragment
  ///     with the desired contents. It should have a signature compatible with:
  ///     pstore::index::fragment_index::value_type(Transaction &, StringAdder
  ///     &).
  /// \returns A pair containing the new fragment's digest and its extent.
  template <typename NameAndLinkageIterator, typename CreateFragmentFn>
  auto compile(NameAndLinkageIterator First, NameAndLinkageIterator Last,
               CreateFragmentFn FragmentCreator)
      -> std::shared_ptr<pstore::repo::compilation const>;

  /// A convenience method which creates a compilation with a single definition
  /// which references a fragment with a read-only section.
  ///
  /// Note that definitions with common linkage _cannot_ be created by this
  /// function.
  ///
  /// \param DefinitionName The name of the definition that is created.
  /// \param Linkage The linkage of the definition to be created. This must not
  /// be "common" linkage.
  auto compile(std::string const &DefinitionName, pstore::repo::linkage Linkage)
      -> std::shared_ptr<pstore::repo::compilation const>;

  /// A convenience method which creates a compilation with a single definition
  /// with common linkage which references a fragment with a BSS section of the
  /// size given by \p CommonSize.
  ///
  /// \param DefinitionName The name of the definition that is created.
  /// \param CommonSize The size of the common symbol.
  auto compileWithCommonSymbol(std::string const &DefinitionName,
                               unsigned CommonSize)
      -> std::shared_ptr<pstore::repo::compilation const>;

  rld::StringAddress storeString(char const *Name);

  static std::shared_ptr<pstore::index::fragment_index>
  getFragmentIndex(pstore::database &Db);
  static std::shared_ptr<pstore::index::compilation_index>
  getCompilationIndex(pstore::database &Db);
  static std::shared_ptr<pstore::index::name_index>
  getNameIndex(pstore::database &Db);

private:
  static pstore::index::fragment_index::value_type
  createFragmentWithReadOnlySection(Transaction &T, StringAdder &N);
  static pstore::index::fragment_index::value_type
  createFragmentWithBSSSection(Transaction &T, size_t Size);

  pstore::database &Db_;
  StringAdder NameAdder_;
};

// compile
// ~~~~~~~
template <typename NameAndLinkageIterator, typename CreateFragmentFn>
std::shared_ptr<pstore::repo::compilation const>
CompilationBuilder::compile(NameAndLinkageIterator First,
                            NameAndLinkageIterator Last,
                            CreateFragmentFn FragmentCreator) {
  static_assert(
      std::is_same<
          typename std::iterator_traits<NameAndLinkageIterator>::value_type,
          NameAndLinkagePair>::value,
      "Iterator must produce NameAndLinkagePair");

  auto FragmentIndex = getFragmentIndex(Db_);
  auto CompilationIndex = getCompilationIndex(Db_);

  using Digest = pstore::index::digest;
  using Compilation = pstore::repo::compilation;
  using CompilationMember = pstore::repo::compilation_member;

  auto Transaction = pstore::begin(Db_);

  std::vector<CompilationMember> Definitions;
  std::transform(
      First, Last, std::back_inserter(Definitions),
      [&FragmentCreator, &Transaction, this](NameAndLinkagePair const &NL) {
        auto const FragmentDigestAndExtent =
            FragmentCreator(Transaction, NameAdder_);
        return CompilationMember{
            FragmentDigestAndExtent.first, FragmentDigestAndExtent.second,
            NameAdder_.add(Transaction, NL.first), NL.second};
      });

  constexpr auto Path = "/path";
  constexpr auto Triple = "machine-vendor-os";
  auto const Result = CompilationIndex->insert(
      Transaction,
      std::make_pair(
          Digest{CompilationIndex->size()},
          Compilation::alloc(Transaction, NameAdder_.add(Transaction, Path),
                             NameAdder_.add(Transaction, Triple),
                             std::begin(Definitions), std::end(Definitions))));
  NameAdder_.flush(Transaction); // Write the bodies of the strings.
  Transaction.commit();

  auto const Iterator = Result.first;
  return pstore::repo::compilation::load(Db_, Iterator->second);
}

#endif // RLD_UNIT_TESTS_COMPILATION_BUILDER_H
