//*  _____         _   ____                  _           _ ____                 _  *
//* |_   _|__  ___| |_/ ___| _   _ _ __ ___ | |__   ___ | |  _ \ ___  ___  ___ | | *
//*   | |/ _ \/ __| __\___ \| | | | '_ ` _ \| '_ \ / _ \| | |_) / _ \/ __|/ _ \| | *
//*   | |  __/\__ \ |_ ___) | |_| | | | | | | |_) | (_) | |  _ <  __/\__ \ (_) | | *
//*   |_|\___||___/\__|____/ \__, |_| |_| |_|_.__/ \___/|_|_| \_\___||___/\___/|_| *
//*                          |___/                                                 *
//*                  *
//* __   _____ _ __  *
//* \ \ / / _ \ '__| *
//*  \ V /  __/ |    *
//*   \_/ \___|_|    *
//*                  *
//===- unit_tests/rld/TestSymbolResolver.cpp ------------------------------===//
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
#include "rld/symbol.h"

#include "CompilationBuilder.h"
#include "EmptyStore.h"
#include "ErrorFn.h"
#include "StringAdder.h"

#include "pstore/core/hamt_set.hpp"
#include "rld/context.h"

#include "gmock/gmock.h"

#include <array>
#include <cassert>
#include <iterator>

using pstore::repo::linkage;

namespace {

//*  ___            _         _ ___                             *
//* / __|_  _ _ __ | |__  ___| / __| __ __ _ _ _  _ _  ___ _ _  *
//* \__ \ || | '  \| '_ \/ _ \ \__ \/ _/ _` | ' \| ' \/ -_) '_| *
//* |___/\_, |_|_|_|_.__/\___/_|___/\__\__,_|_||_|_||_\___|_|   *
//*      |__/                                                   *
class SymbolScanner : public testing::Test, public EmptyStore {
public:
  SymbolScanner() : CompilationBuilder_{this->Db()} {}

protected:
  static rld::StringAddress getStringAddress(pstore::database const &Db,
                                             char const *Name);

  static void checkFragmentContents(
      std::shared_ptr<pstore::repo::fragment const> const &Fragment,
      uint8_t Index, pstore::repo::linkage Linkage);

  /// A function which will create a compilation containing a single definition
  /// with a particular name and linkage. There's unavoidable logic here because
  /// definitions with common linkage _must_ contain a sole BSS section.
  std::shared_ptr<pstore::repo::compilation const>
  compile(std::string const &DefinitionName, pstore::repo::linkage Linkage);

  CompilationBuilder CompilationBuilder_;
};

// get string address
// ~~~~~~~~~~~~~~~~~~
rld::StringAddress SymbolScanner::getStringAddress(pstore::database const &Db,
                                                   char const *Name) {
  auto NameIndex = pstore::index::get_index<pstore::trailer::indices::name>(Db);
  pstore::raw_sstring_view const str = pstore::make_sstring_view(Name);
  auto Pos = NameIndex->find(Db, pstore::indirect_string{Db, &str});
  assert(Pos != NameIndex->end(Db));
  return rld::StringAddress::make(Pos.get_address());
}

// compile
// ~~~~~~~
std::shared_ptr<pstore::repo::compilation const>
SymbolScanner::compile(std::string const &DefinitionName,
                       pstore::repo::linkage Linkage) {
  if (Linkage == pstore::repo::linkage::common) {
    return CompilationBuilder_.compileWithCommonSymbol(DefinitionName,
                                                       1U /*size*/);
  }
  return CompilationBuilder_.compile(DefinitionName, Linkage);
}

// check fragment contents [static]
// ~~~~~~~~~~~~~~~~~~~~~~~
void SymbolScanner::checkFragmentContents(
    std::shared_ptr<pstore::repo::fragment const> const &Fragment,
    uint8_t Index, pstore::repo::linkage Linkage) {
  ASSERT_NE(Fragment.get(), nullptr);
  EXPECT_EQ(Fragment->size(), 1U);
  if (Linkage == pstore::repo::linkage::common) {
    pstore::repo::bss_section const *const bss =
        Fragment->atp<pstore::repo::section_kind::bss>();
    ASSERT_NE(bss, nullptr) << "Expected the fragment to have a BSS section";
  } else {
    pstore::repo::generic_section const *const rodata =
        Fragment->atp<pstore::repo::section_kind::read_only>();
    ASSERT_NE(rodata, nullptr)
        << "Expected the fragment to have a read-only section";
    EXPECT_THAT(rodata->payload(), testing::ElementsAre(Index))
        << "Expected fragment #" << unsigned{Index};
  }
}

//*  ____  _             _      ____                  _           _  *
//* / ___|(_)_ __   __ _| | ___/ ___| _   _ _ __ ___ | |__   ___ | | *
//* \___ \| | '_ \ / _` | |/ _ \___ \| | | | '_ ` _ \| '_ \ / _ \| | *
//*  ___) | | | | | (_| | |  __/___) | |_| | | | | | | |_) | (_) | | *
//* |____/|_|_| |_|\__, |_|\___|____/ \__, |_| |_| |_|_.__/ \___/|_| *
//*                |___/              |___/                          *
class SingleSymbol : public SymbolScanner,
                     public testing::WithParamInterface<linkage> {
public:
  SingleSymbol() : Ctx_{this->Db()}, Resolver_{Ctx_} {}

protected:
  rld::Context Ctx_;
  rld::SymbolResolver Resolver_;
};

} // end anonymous namespace

TEST_P(SingleSymbol, SingleSymbol) {
  linkage const Linkage = this->GetParam();
  using testing::_;
  using ReturnType = llvm::Optional<rld::LocalSymbolsContainer>;

  ErrorFn ErrorCallback;
  EXPECT_CALL(ErrorCallback, invoke(_)).Times(0);

  constexpr auto InputOrdinal =
      uint32_t{43}; // (Using an unimportant small prime.)

  rld::UndefsContainer Undefs;
  // Simulate a CU containing a single symbol ("f0") of type 'Linkage'. Create a
  // symbol resolver and get it to examine our new compilation.
  rld::GlobalSymbolsContainer Globals;
  ReturnType const C0Locals =
      Resolver_.defineSymbols(&Globals, &Undefs, *compile("f0", Linkage),
                              InputOrdinal, std::cref(ErrorCallback));

  ASSERT_TRUE(C0Locals.hasValue());
  EXPECT_TRUE(Undefs.empty());

  {
    ASSERT_EQ(Globals.size(), 1U);
    rld::Symbol const &Sym = *Globals.begin();
    ASSERT_TRUE(Sym.hasDefinition()) << "The symbol must be be defined";
    auto DefinitionAndLock = Sym.definition();
    auto const &Bodies =
        std::get<rld::Symbol::OptionalBodies const &>(DefinitionAndLock);
    auto const &Lock =
        std::get<std::unique_lock<rld::Symbol::Mutex>>(DefinitionAndLock);
    ASSERT_TRUE(Lock.owns_lock()) << "definition() must return an owned lock";
    ASSERT_TRUE(Bodies.hasValue()) << "The symbol should be defined [and must "
                                      "agree with Sym.hasDefinition()]";

    ASSERT_EQ(Bodies->size(), 1U);
    rld::Symbol::Body const &B = Bodies->front();
    EXPECT_EQ(B.inputOrdinal(), InputOrdinal);
    EXPECT_EQ(B.linkage(), Linkage);

    SCOPED_TRACE("SingleSymbol,SingleSymbol");
    this->checkFragmentContents(B.fragment(), uint8_t{0}, Linkage);
  }
  // Now that the CU-local view of the symbols is correct.
  {
    ASSERT_EQ(C0Locals->size(), 1U);
    rld::LocalSymbolsContainer::value_type const &S = *C0Locals->begin();
    EXPECT_EQ(rld::loadStdString(this->Db(), S.first), "f0");
    EXPECT_EQ(S.second, &*Globals.begin());
  }
}

INSTANTIATE_TEST_CASE_P(
    LinkageType, SingleSymbol,
    testing::Values(linkage::append, linkage::common, linkage::external,
                    linkage::internal_no_symbol, linkage::internal,
                    linkage::link_once_any, linkage::link_once_odr,
                    linkage::weak_any, linkage::weak_odr), );

//*  _____              ___            _         _     *
//* |_   _|_ __ _____  / __|_  _ _ __ | |__  ___| |___ *
//*   | | \ V  V / _ \ \__ \ || | '  \| '_ \/ _ \ (_-< *
//*   |_|  \_/\_/\___/ |___/\_, |_|_|_|_.__/\___/_/__/ *
//*                         |__/                       *
namespace {

class TwoSymbols : public SymbolScanner {
public:
  TwoSymbols() : Ctx_{this->Db()}, Resolver_{Ctx_} {}

protected:
  using ReturnType = llvm::Optional<rld::LocalSymbolsContainer>;
  void checkSymbol(rld::Symbol const &Symbol, unsigned TicketFileIndex,
                   linkage Linkage, uint8_t FragmentIndex);
  rld::Symbol const &getSymbol(rld::GlobalSymbolsContainer const &Globals,
                               unsigned Index);

  static constexpr uint32_t Input0_ = 0;
  static constexpr uint32_t Input1_ = 1;

  void checkCompilationLocalView(
      llvm::Optional<rld::LocalSymbolsContainer> const &LocalView,
      rld::Symbol const &S) const;

  rld::Context Ctx_;
  rld::SymbolResolver Resolver_;
};

constexpr uint32_t TwoSymbols::Input0_;
constexpr uint32_t TwoSymbols::Input1_;

// get symbol
// ~~~~~~~~~~
rld::Symbol const &
TwoSymbols::getSymbol(rld::GlobalSymbolsContainer const &Globals,
                      unsigned Index) {
  // auto const &ST = Ctx_.symbolTable();
  assert(Index < Globals.size());
  auto It = std::begin(Globals);
  std::advance(It, Index);
  return *It;
}

// check symbol
// ~~~~~~~~~~~~
void TwoSymbols::checkSymbol(const rld::Symbol &Symbol,
                             uint32_t TicketFileOrdinal, linkage Linkage,
                             uint8_t FragmentIndex) {
  ASSERT_TRUE(Symbol.hasDefinition()) << "The symbol must be be defined";
  auto DefinitionAndLock = Symbol.definition();
  auto const &Bodies =
      std::get<rld::Symbol::OptionalBodies const &>(DefinitionAndLock);
  auto const &Lock =
      std::get<std::unique_lock<rld::Symbol::Mutex>>(DefinitionAndLock);
  ASSERT_TRUE(Lock.owns_lock()) << "definition() must return an owned lock";
  ASSERT_TRUE(Bodies.hasValue()) << "The symbol should be defined [and must "
                                    "agree with Symbol.hasDefinition()]";
  ASSERT_EQ(Bodies->size(), 1U) << "Symbol must have a single body";
  const rld::Symbol::Body &B = Bodies->front();
  EXPECT_EQ(B.inputOrdinal(), TicketFileOrdinal);
  EXPECT_EQ(B.linkage(), Linkage);
  // Ensure that we have the expected fragment associated with this symbol. We
  // create fragments with a single section whose data payload is a single byte
  // derived from the fragment-index size at the time it was created.
  this->checkFragmentContents(B.fragment(), FragmentIndex, Linkage);
}

// check compilation local view
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
void TwoSymbols::checkCompilationLocalView(
    llvm::Optional<rld::LocalSymbolsContainer> const &LocalView,
    rld::Symbol const &S) const {
  // Check the CU-local view of the symbols is correct for this
  // compilation.
  ASSERT_TRUE(LocalView.hasValue());
  EXPECT_EQ(LocalView->size(), 1U);
  EXPECT_EQ(LocalView->begin()->second, &S);
}

using TwoLinkages = std::tuple<linkage, linkage>;

//*  _____               ___                _ _      _   _              *
//* |_   _|_ __ _____   / __|___ _ __  _ __(_) |__ _| |_(_)___ _ _  ___ *
//*   | | \ V  V / _ \ | (__/ _ \ '  \| '_ \ | / _` |  _| / _ \ ' \(_-< *
//*   |_|  \_/\_/\___/  \___\___/_|_|_| .__/_|_\__,_|\__|_\___/_||_/__/ *
//*                                   |_|                               *
class TwoCompilations : public TwoSymbols,
                        public testing::WithParamInterface<TwoLinkages> {
public:
  TwoCompilations();

protected:
  std::shared_ptr<pstore::repo::compilation const> CU0_;
  std::shared_ptr<pstore::repo::compilation const> CU1_;

  void checkSuccess(ReturnType const &C0, ReturnType const &C1,
                    rld::Symbol const &Symbol0) const;
};

// ctor
// ~~~~
TwoCompilations::TwoCompilations() {
  CU0_ = this->compile("f", std::get<0>(GetParam()));
  CU1_ = this->compile("f", std::get<1>(GetParam()));
}

// check success
// ~~~~~~~~~~~~~
void TwoCompilations::checkSuccess(ReturnType const &C0, ReturnType const &C1,
                                   rld::Symbol const &Symbol0) const {

  // Now check the CU-local view of the symbols is correct for the first
  // compilation.
  ASSERT_TRUE(C0.hasValue());
  EXPECT_EQ(C0->size(), 1U);
  EXPECT_EQ(C0->begin()->second, &Symbol0);

  // Now check the CU-local view of the symbols is correct for the second
  // compilation.
  ASSERT_TRUE(C1.hasValue());
  EXPECT_EQ(C1->size(), 1U);
  EXPECT_EQ(C1->begin()->second, &Symbol0);
}

} // end anonymous namespace

using testing::_;
using testing::Invoke;

//*  _                      _      ___         _ _           _  *
//* | |   _____ __ _____ __| |_   / _ \ _ _ __| (_)_ _  __ _| | *
//* | |__/ _ \ V  V / -_|_-<  _| | (_) | '_/ _` | | ' \/ _` | | *
//* |____\___/\_/\_/\___/__/\__|  \___/|_| \__,_|_|_||_\__,_|_| *
//*                                                             *

class LowestOrdinal : public TwoCompilations {};

TEST_P(LowestOrdinal, LowerOrdinalFirst) {
  // No errors.
  ErrorFn ErrorCallback;
  EXPECT_CALL(ErrorCallback, invoke(_)).Times(0);

  rld::UndefsContainer Undefs;
  rld::GlobalSymbolsContainer Globals;

  // Define the symbol in the first compilation (Input0_) before the second
  // (Input1_).
  ReturnType const C0 = Resolver_.defineSymbols(
      &Globals, &Undefs, *CU0_, Input0_, std::cref(ErrorCallback));
  ASSERT_TRUE(C0.hasValue());
  ReturnType const C1 = Resolver_.defineSymbols(
      &Globals, &Undefs, *CU1_, Input1_, std::cref(ErrorCallback));
  ASSERT_TRUE(C1.hasValue());

  // Check that the resolver did the right thing. First that the global symbol
  // table is as expected.
  ASSERT_EQ(Globals.size(), 1U);
  EXPECT_TRUE(Undefs.empty());

  {
    SCOPED_TRACE("LowestOrdinal, LowerOrdinalFirst");
    rld::Symbol const &Symbol0 = this->getSymbol(Globals, 0U);
    this->checkSymbol(Symbol0, Input0_, std::get<0>(GetParam()), 0U);
    // The CU-local symbol view should show that the symbol from CU0 was picked.
    this->checkSuccess(C0, C1, Symbol0);
  }
}

TEST_P(LowestOrdinal, LowerOrdinalSecond) {
  // No errors.
  ErrorFn ErrorCallback;
  EXPECT_CALL(ErrorCallback, invoke(_)).Times(0);

  rld::UndefsContainer Undefs;
  rld::GlobalSymbolsContainer Globals;
  // Define the symbol in the second compilation (Input1_) before the first
  // (Input0_).
  ReturnType C1 = Resolver_.defineSymbols(&Globals, &Undefs, *CU1_, Input1_,
                                          std::cref(ErrorCallback));
  ASSERT_TRUE(C1);
  ReturnType C0 = Resolver_.defineSymbols(&Globals, &Undefs, *CU0_, Input0_,
                                          std::cref(ErrorCallback));
  ASSERT_TRUE(C0);

  // Check that the resolver did the right thing. First that the global symbol
  // table is as expected.
  ASSERT_EQ(Globals.size(), 1U);
  EXPECT_TRUE(Undefs.empty());

  {
    SCOPED_TRACE("LowestOrdinal, LowerOrdinalSecond");
    rld::Symbol const &Symbol0 = this->getSymbol(Globals, 0U);
    this->checkSymbol(Symbol0, Input0_, std::get<0>(GetParam()), 0U);

    // The CU-local symbol view should show that the symbol from CU0 was picked.
    this->checkSuccess(C0, C1, Symbol0);
  }
}

// Checks cases where we have an existing link-once definition and then see a
// second.
INSTANTIATE_TEST_CASE_P(
    LinkOnce, LowestOrdinal,
    testing::Values(TwoLinkages{linkage::link_once_any, linkage::link_once_any},
                    TwoLinkages{linkage::link_once_any, linkage::link_once_odr},
                    TwoLinkages{linkage::link_once_odr, linkage::link_once_odr},
                    TwoLinkages{linkage::link_once_odr,
                                linkage::link_once_any}), );

// Checks cases where we have an existing weak definition and then see a second.
INSTANTIATE_TEST_CASE_P(
    Weak, LowestOrdinal,
    testing::Values(TwoLinkages{linkage::weak_any, linkage::weak_any},
                    TwoLinkages{linkage::weak_any, linkage::weak_odr},
                    TwoLinkages{linkage::weak_odr, linkage::weak_odr},
                    TwoLinkages{linkage::weak_odr, linkage::weak_any}), );

//*  ___          _                 *
//* | _ \___ _ __| |__ _ __ ___ ___ *
//* |   / -_) '_ \ / _` / _/ -_|_-< *
//* |_|_\___| .__/_\__,_\__\___/__/ *
//*         |_|                     *
class Replaces : public TwoCompilations {};

TEST_P(Replaces, Replaces) {
  // No errors.
  ErrorFn ErrorCallback;
  EXPECT_CALL(ErrorCallback, invoke(_)).Times(0);

  rld::UndefsContainer Undefs;
  rld::GlobalSymbolsContainer Globals;

  // Two compilations defining the same name: the first with a symbol linkage
  // from the first test parameter, the second with the second parameter symbol
  // linkage.
  ReturnType C0 = Resolver_.defineSymbols(&Globals, &Undefs, *CU0_, Input0_,
                                          std::cref(ErrorCallback));
  ASSERT_TRUE(C0) << "Expected symbol resolution for CU0 to succeed";
  ReturnType C1 = Resolver_.defineSymbols(&Globals, &Undefs, *CU1_, Input1_,
                                          std::cref(ErrorCallback));
  ASSERT_TRUE(C1) << "Expected symbol resolution for CU1 to succeed";

  // Check that the resolver did the right thing. First that the global symbol
  // table is as expected.
  ASSERT_EQ(Globals.size(), 1U);
  EXPECT_TRUE(Undefs.empty());

  rld::Symbol const &Symbol0 = this->getSymbol(Globals, 0U);
  {
    SCOPED_TRACE("Replaces");
    this->checkSymbol(Symbol0, Input1_, std::get<1>(GetParam()), 1U);
    // Both compilations should see a definition of Symbol0.
    this->checkSuccess(C0, C1, Symbol0);
  }
}

INSTANTIATE_TEST_CASE_P(Common, Replaces,
                        testing::Values(TwoLinkages{linkage::common,
                                                    linkage::external}), );

INSTANTIATE_TEST_CASE_P(
    Weak, Replaces,
    testing::Values(TwoLinkages{linkage::weak_any, linkage::common},
                    TwoLinkages{linkage::weak_odr, linkage::common},
                    TwoLinkages{linkage::weak_any, linkage::external},
                    TwoLinkages{linkage::weak_odr, linkage::external}), );

//*  ___                          _  *
//* |_ _|__ _ _ _  ___ _ _ ___ __| | *
//*  | |/ _` | ' \/ _ \ '_/ -_) _` | *
//* |___\__, |_||_\___/_| \___\__,_| *
//*     |___/                        *

class Ignored : public TwoCompilations {};

TEST_P(Ignored, Ignored) {
  ErrorFn ErrorCallback;
  EXPECT_CALL(ErrorCallback, invoke(_)).Times(0);

  rld::UndefsContainer Undefs;
  rld::GlobalSymbolsContainer Globals;

  // Two compilations defining the same name: the first with a symbol linkage
  // from the first test parameter, the second with the second parameter symbol
  // linkage.
  ReturnType C0 = Resolver_.defineSymbols(&Globals, &Undefs, *CU0_, Input0_,
                                          std::cref(ErrorCallback));
  ASSERT_TRUE(C0);
  ReturnType C1 = Resolver_.defineSymbols(&Globals, &Undefs, *CU1_, Input1_,
                                          std::cref(ErrorCallback));
  ASSERT_TRUE(C1);

  // Check that the resolver did the right thing. First that the global symbol
  // table is as expected.
  ASSERT_EQ(Globals.size(), 1U);
  EXPECT_TRUE(Undefs.empty());

  rld::Symbol const &Symbol0 = this->getSymbol(Globals, 0U);
  {
    SCOPED_TRACE("Ignored");
    this->checkSymbol(Symbol0, Input0_, std::get<0>(GetParam()), 0U);
    // Both compilations should see a definition of Symbol0.
    this->checkSuccess(C0, C1, Symbol0);
  }
}

// Simulate cases where we have an external definition and we then see a common
// or weak.
INSTANTIATE_TEST_CASE_P(
    External, Ignored,
    testing::Values(TwoLinkages{linkage::external, linkage::common},
                    TwoLinkages{linkage::external, linkage::weak_any},
                    TwoLinkages{linkage::external, linkage::weak_odr}), );

// Simulate cases where we have a common definition and we then see a weak.
INSTANTIATE_TEST_CASE_P(
    Common, Ignored,
    testing::Values(TwoLinkages{linkage::common, linkage::weak_any},
                    TwoLinkages{linkage::common, linkage::weak_odr}), );

//*   ___     _ _ _    _           *
//*  / __|___| | (_)__(_)___ _ _   *
//* | (__/ _ \ | | (_-< / _ \ ' \  *
//*  \___\___/_|_|_/__/_\___/_||_| *
//*                                *

class Collision : public TwoCompilations {};

TEST_P(Collision, OtherHits) {
  ErrorFn ErrorCallback;
  EXPECT_CALL(ErrorCallback, invoke(_))
      .WillOnce(Invoke([this](rld::StringAddress Addr) {
        EXPECT_EQ(rld::loadStdString(this->Db(), Addr), "f");
      }));

  rld::UndefsContainer Undefs;
  rld::GlobalSymbolsContainer Globals;

  // Two compilations defining the same name: the first with an symbol linkage
  // from the first test parameter, the second with the second parameter symbol
  // linkage.
  ReturnType C0 = Resolver_.defineSymbols(&Globals, &Undefs, *CU0_, Input0_,
                                          std::cref(ErrorCallback));
  ReturnType C1 = Resolver_.defineSymbols(&Globals, &Undefs, *CU1_, Input1_,
                                          std::cref(ErrorCallback));

  // Check that the resolver did the right thing. First that the global symbol
  // table is as expected.
  ASSERT_EQ(Globals.size(), 1U);
  EXPECT_TRUE(Undefs.empty());

  rld::Symbol const &Symbol0 = this->getSymbol(Globals, 0U);
  {
    SCOPED_TRACE("Collision");
    this->checkSymbol(Symbol0, Input0_, std::get<0>(GetParam()), 0U);
  }

  // The first compilation should see a definition of Symbol0.
  ASSERT_TRUE(C0.hasValue());
  EXPECT_EQ(C0->size(), 1U);
  EXPECT_EQ(C0->begin()->second, &Symbol0);

  // Now check that the second compilation had an error.
  EXPECT_FALSE(C1.hasValue());
}

// Symbols colliding with an append definition.
INSTANTIATE_TEST_CASE_P(
    Collision, Collision,
    testing::Values(TwoLinkages{linkage::append, linkage::common},
                    TwoLinkages{linkage::append, linkage::external},
                    TwoLinkages{linkage::append, linkage::link_once_any},
                    TwoLinkages{linkage::append, linkage::link_once_odr},
                    TwoLinkages{linkage::append, linkage::weak_any},
                    TwoLinkages{linkage::append, linkage::weak_odr}), );

// Symbols colliding with a common definition.
INSTANTIATE_TEST_CASE_P(
    Common, Collision,
    testing::Values(TwoLinkages{linkage::common, linkage::append},
                    TwoLinkages{linkage::common, linkage::link_once_any},
                    TwoLinkages{linkage::common, linkage::link_once_odr}), );

// Symbols colliding with an external definition.
INSTANTIATE_TEST_CASE_P(
    External, Collision,
    testing::Values(TwoLinkages{linkage::external, linkage::append},
                    TwoLinkages{linkage::external, linkage::external},
                    TwoLinkages{linkage::external, linkage::link_once_any},
                    TwoLinkages{linkage::external, linkage::link_once_odr}), );

// Symbols colliding with a link-once-any or link-once-ODR definition.
INSTANTIATE_TEST_CASE_P(
    LinkOnce, Collision,
    testing::Values(TwoLinkages{linkage::link_once_any, linkage::append},
                    TwoLinkages{linkage::link_once_any, linkage::common},
                    TwoLinkages{linkage::link_once_any, linkage::external},
                    TwoLinkages{linkage::link_once_any, linkage::weak_any},
                    TwoLinkages{linkage::link_once_any, linkage::weak_odr},
                    TwoLinkages{linkage::link_once_odr, linkage::append},
                    TwoLinkages{linkage::link_once_odr, linkage::common},
                    TwoLinkages{linkage::link_once_odr, linkage::external},
                    TwoLinkages{linkage::link_once_odr, linkage::weak_any},
                    TwoLinkages{linkage::link_once_odr, linkage::weak_odr}), );

// Symbols colliding with a weak-any or weak-ODR definition.
INSTANTIATE_TEST_CASE_P(
    Weak, Collision,
    testing::Values(TwoLinkages{linkage::weak_any, linkage::append},
                    TwoLinkages{linkage::weak_any, linkage::link_once_any},
                    TwoLinkages{linkage::weak_any, linkage::link_once_odr},
                    TwoLinkages{linkage::weak_odr, linkage::append},
                    TwoLinkages{linkage::weak_odr, linkage::link_once_any},
                    TwoLinkages{linkage::weak_odr, linkage::link_once_odr}), );

namespace {

class Append : public TwoSymbols {
public:
  Append()
      : CU0_{CompilationBuilder_.compile("f", linkage::append)},
        CU1_{CompilationBuilder_.compile("f", linkage::append)} {}

protected:
  void checkSymbolTableEntry(rld::Symbol const &Symbol0);
  void checkLocalSymbolView(ReturnType const &C0, ReturnType const &C1,
                            rld::Symbol const &Symbol) const;

  std::shared_ptr<pstore::repo::compilation const> const CU0_;
  std::shared_ptr<pstore::repo::compilation const> const CU1_;
};

// check symbol table entry
// ~~~~~~~~~~~~~~~~~~~~~~~~
void Append::checkSymbolTableEntry(rld::Symbol const &Symbol0) {
  ASSERT_TRUE(Symbol0.hasDefinition()) << "The symbol must be be defined";
  auto DefinitionAndLock = Symbol0.definition();
  auto const &Bodies =
      std::get<rld::Symbol::OptionalBodies const &>(DefinitionAndLock);
  auto const &Lock =
      std::get<std::unique_lock<rld::Symbol::Mutex>>(DefinitionAndLock);
  ASSERT_TRUE(Lock.owns_lock()) << "definition() must return an owned lock";
  ASSERT_TRUE(Bodies.hasValue()) << "The symbol should be defined [and must "
                                    "agree with Symbol.hasDefinition()]";
  ASSERT_EQ(Bodies->size(), 2U) << "Symbol must have two bodies";
  {
    const rld::Symbol::Body &B0 = (*Bodies)[0];
    EXPECT_EQ(B0.inputOrdinal(), Input0_)
        << "Body 0 must be from the first compilation";
    EXPECT_EQ(B0.linkage(), linkage::append);
    // Ensure that we have the expected fragment associated with this symbol. We
    // create fragments with a single section whose data payload is a single
    // byte derived from the fragment-index size at the time it was created.
    this->checkFragmentContents(B0.fragment(), 0, linkage::append);
  }
  {
    const rld::Symbol::Body &B1 = (*Bodies)[1];
    EXPECT_EQ(B1.inputOrdinal(), Input1_)
        << "Body 1 must be from the second compilation";
    EXPECT_EQ(B1.linkage(), linkage::append);
    this->checkFragmentContents(B1.fragment(), 1, linkage::append);
  }
}

// check local symbol view
// ~~~~~~~~~~~~~~~~~~~~~~~
void Append::checkLocalSymbolView(ReturnType const &C0, ReturnType const &C1,
                                  rld::Symbol const &Symbol) const {
  // Now check the CU-local view of the symbols is correct for the first
  // compilation.
  ASSERT_TRUE(C0.hasValue());
  EXPECT_EQ(C0->size(), 1U);
  EXPECT_EQ(C0->begin()->second, &Symbol);

  // Now check the CU-local view of the symbols is correct for the second
  // compilation.
  ASSERT_TRUE(C1.hasValue());
  EXPECT_EQ(C1->size(), 1U);
  EXPECT_EQ(C1->begin()->second, &Symbol);
}

} // end anonymous namespace

TEST_F(Append, LowerOrdinalFirst) {
  // No errors.
  ErrorFn ErrorCallback;
  EXPECT_CALL(ErrorCallback, invoke(_)).Times(0);

  rld::UndefsContainer Undefs;
  rld::GlobalSymbolsContainer Globals;

  // Two compilations defining the same name with append linkage.
  ReturnType C0 = Resolver_.defineSymbols(&Globals, &Undefs, *CU0_, Input0_,
                                          std::cref(ErrorCallback));
  ASSERT_TRUE(C0);
  ReturnType C1 = Resolver_.defineSymbols(&Globals, &Undefs, *CU1_, Input1_,
                                          std::cref(ErrorCallback));
  ASSERT_TRUE(C1);

  ASSERT_EQ(Globals.size(), 1U) << "A single symbol table entry is expected";
  EXPECT_TRUE(Undefs.empty()) << "There should be no undefined symbols";
  rld::Symbol const &Symbol = this->getSymbol(Globals, 0U);
  checkSymbolTableEntry(Symbol);
  checkLocalSymbolView(C0, C1, Symbol);
}

TEST_F(Append, LowerOrdinalSecond) {
  // No errors.
  ErrorFn ErrorCallback;
  EXPECT_CALL(ErrorCallback, invoke(_)).Times(0);

  rld::UndefsContainer Undefs;
  rld::GlobalSymbolsContainer Globals;

  // Two compilations defining the same name with append linkage.
  ReturnType C1 = Resolver_.defineSymbols(&Globals, &Undefs, *CU1_, Input1_,
                                          std::cref(ErrorCallback));
  ASSERT_TRUE(C1);
  ReturnType C0 = Resolver_.defineSymbols(&Globals, &Undefs, *CU0_, Input0_,
                                          std::cref(ErrorCallback));
  ASSERT_TRUE(C0);

  ASSERT_EQ(Globals.size(), 1U) << "A single symbol table entry is expected";
  EXPECT_TRUE(Undefs.empty()) << "There should be no undefined symbols";
  rld::Symbol const &Symbol = this->getSymbol(Globals, 0U);
  checkSymbolTableEntry(Symbol);
  checkLocalSymbolView(C0, C1, Symbol);
}

//*  _                         _    *
//* | |   __ _ _ _ __ _ ___ __| |_  *
//* | |__/ _` | '_/ _` / -_|_-<  _| *
//* |____\__,_|_| \__, \___/__/\__| *
//*               |___/             *
namespace {

class Largest : public TwoSymbols {
protected:
  //  using ReturnType = llvm::Optional<rld::LocalSymbolsContainer>;
  static constexpr auto OrdinalA_ = uint32_t{43};
  static constexpr auto OrdinalB_ = uint32_t{47};
  static constexpr auto Name_ = "f";

  void checkCommonSymbol(rld::Symbol const &Sym, std::size_t ExpectedOrdinal,
                         std::size_t ExpectedSize);
};

// checkCommonSymbol
// ~~~~~~~~~~~~~~~~~
void Largest::checkCommonSymbol(rld::Symbol const &Sym,
                                std::size_t ExpectedOrdinal,
                                std::size_t ExpectedSize) {
  ASSERT_TRUE(Sym.hasDefinition()) << "The symbol must be defined";
  auto DefinitionAndLock = Sym.definition();
  auto const &Bodies =
      std::get<rld::Symbol::OptionalBodies const &>(DefinitionAndLock);
  auto const &Lock =
      std::get<std::unique_lock<rld::Symbol::Mutex>>(DefinitionAndLock);
  ASSERT_TRUE(Lock.owns_lock()) << "definition() must return an owned lock";
  ASSERT_TRUE(Bodies.hasValue()) << "The symbol should be defined [and must "
                                    "agree with Sym.hasDefinition()]";

  ASSERT_EQ(Bodies->size(), 1U)
      << "There must be a single definition of the symbol";

  auto const &Body = Bodies->front();
  EXPECT_EQ(Body.linkage(), linkage::common)
      << "Expected the symbol to have common linkage";
  EXPECT_EQ(Body.inputOrdinal(), ExpectedOrdinal)
      << "The InputOrdinal was not correct";

  auto const &Fragment = Body.fragment();
  ASSERT_NE(Fragment.get(), nullptr);
  EXPECT_EQ(Fragment->size(), 1U)
      << "Expected the fragment to contain a single section";
  auto const *const BSS = Fragment->atp<pstore::repo::section_kind::bss>();
  ASSERT_NE(BSS, nullptr);
  EXPECT_EQ(BSS->size(), ExpectedSize) << "The fragment size was not correct";
}

} // end anonymous namespace

TEST_F(Largest, ALtB) {
  // No errors are expected.
  ErrorFn ErrorCallback;
  EXPECT_CALL(ErrorCallback, invoke(_)).Times(0);

  constexpr auto SizeA = 1U;
  constexpr auto SizeB = 2U;
  auto CompilationA = CompilationBuilder_.compileWithCommonSymbol(Name_, SizeA);
  auto CompilationB = CompilationBuilder_.compileWithCommonSymbol(Name_, SizeB);

  rld::UndefsContainer Undefs;
  rld::GlobalSymbolsContainer Globals;

  ReturnType CA = Resolver_.defineSymbols(&Globals, &Undefs, *CompilationA,
                                          OrdinalA_, std::cref(ErrorCallback));
  ASSERT_TRUE(CA.hasValue())
      << "Symbol resolution for OrdinalA_ produced an error";
  EXPECT_EQ(Globals.size(), 1U)
      << "The global symbol table should hold 1 entry";

  ReturnType CB = Resolver_.defineSymbols(&Globals, &Undefs, *CompilationB,
                                          OrdinalB_, std::cref(ErrorCallback));

  EXPECT_TRUE(Undefs.empty());
  ASSERT_TRUE(CB.hasValue())
      << "Symbol resolution for OrdinalB_ produced an error";
  ASSERT_EQ(CB->size(), 1U) << "The global symbol table should hold 1 entry";

  SCOPED_TRACE("Largest,ALtB");
  rld::Symbol const &Sym = *Globals.begin();
  this->checkCommonSymbol(Sym, OrdinalB_, SizeB);
}

TEST_F(Largest, AGtB) {
  // No errors are expected.
  ErrorFn ErrorCallback;
  EXPECT_CALL(ErrorCallback, invoke(_)).Times(0);

  constexpr auto SizeA = 2U;
  constexpr auto SizeB = 1U;
  auto CompilationA = CompilationBuilder_.compileWithCommonSymbol(Name_, SizeA);
  auto CompilationB = CompilationBuilder_.compileWithCommonSymbol(Name_, SizeB);
  rld::UndefsContainer Undefs;
  rld::GlobalSymbolsContainer Globals;

  ReturnType CA = Resolver_.defineSymbols(&Globals, &Undefs, *CompilationA,
                                          OrdinalA_, std::cref(ErrorCallback));
  ASSERT_TRUE(CA.hasValue());
  ReturnType CB = Resolver_.defineSymbols(&Globals, &Undefs, *CompilationB,
                                          OrdinalB_, std::cref(ErrorCallback));

  EXPECT_TRUE(Undefs.empty());
  ASSERT_TRUE(CB.hasValue());
  // Check that we have a single entry in the symbol table.
  ASSERT_EQ(Globals.size(), 1U) << "Expected a single global symbol";

  SCOPED_TRACE("Largest,ALtB");
  this->checkCommonSymbol(*Globals.begin(), OrdinalA_, SizeA);
}

TEST_F(Largest, AEqBLowestOrdinalFirst) {
  // No errors are expected.
  ErrorFn ErrorCallback;
  EXPECT_CALL(ErrorCallback, invoke(_)).Times(0);

  constexpr auto Size = 1U;
  auto CompilationA = CompilationBuilder_.compileWithCommonSymbol(Name_, Size);
  auto CompilationB = CompilationBuilder_.compileWithCommonSymbol(Name_, Size);
  rld::UndefsContainer Undefs;
  rld::GlobalSymbolsContainer Globals;
  ReturnType CA = Resolver_.defineSymbols(&Globals, &Undefs, *CompilationA,
                                          OrdinalA_, std::cref(ErrorCallback));
  ASSERT_TRUE(CA.hasValue());
  ReturnType CB = Resolver_.defineSymbols(&Globals, &Undefs, *CompilationB,
                                          OrdinalB_, std::cref(ErrorCallback));

  EXPECT_TRUE(Undefs.empty());
  ASSERT_TRUE(CB.hasValue());

  // Check that we have a single entry in the symbol table.
  ASSERT_EQ(Globals.size(), 1U) << "Expected a single global symbol";

  SCOPED_TRACE("Largest,AEqBLowestOrdinalFirst");
  this->checkCommonSymbol(*Globals.begin(), OrdinalA_, Size);
}

TEST_F(Largest, AEqBHighestOrdinalFirst) {
  // No errors are expected.
  ErrorFn ErrorCallback;
  EXPECT_CALL(ErrorCallback, invoke(_)).Times(0);

  constexpr auto Size = 1U;
  auto CompilationA = CompilationBuilder_.compileWithCommonSymbol(Name_, Size);
  auto CompilationB = CompilationBuilder_.compileWithCommonSymbol(Name_, Size);
  rld::UndefsContainer Undefs;
  rld::GlobalSymbolsContainer Globals;
  // Define the symbol from the file with the higher ordinal _first_,
  ReturnType CB = Resolver_.defineSymbols(&Globals, &Undefs, *CompilationA,
                                          OrdinalB_, std::cref(ErrorCallback));
  ASSERT_TRUE(CB.hasValue());
  ReturnType CA = Resolver_.defineSymbols(&Globals, &Undefs, *CompilationB,
                                          OrdinalA_, std::cref(ErrorCallback));

  EXPECT_TRUE(Undefs.empty());
  ASSERT_TRUE(CA.hasValue());

  // Check that we have a single entry in the symbol table.
  ASSERT_EQ(Globals.size(), 1U);

  SCOPED_TRACE("Largest,AEqBHighestOrdinalFirst");
  this->checkCommonSymbol(*Globals.begin(), OrdinalA_, Size);
}

//*  ___      __   ___       __               ___       __  *
//* | _ \___ / _| | _ ) ___ / _|___ _ _ ___  |   \ ___ / _| *
//* |   / -_)  _| | _ \/ -_)  _/ _ \ '_/ -_) | |) / -_)  _| *
//* |_|_\___|_|   |___/\___|_| \___/_| \___| |___/\___|_|   *
//*                                                         *
namespace {

class RefBeforeDef : public TwoSymbols,
                     public testing::WithParamInterface<linkage> {};

} // end anonymous namespace

TEST_P(RefBeforeDef, DefinitionReplacesReference) {
  linkage const Linkage = this->GetParam();
  static constexpr auto Name = "f";

  ErrorFn ErrorCallback;
  EXPECT_CALL(ErrorCallback, invoke(_)).Times(0);

  // Create a reference to "f".
  rld::UndefsContainer Undefs;
  rld::GlobalSymbolsContainer Globals;
  rld::LocalSymbolsContainer S0;
  rld::referenceSymbol(Ctx_, CompilationBuilder_.storeString(Name), S0,
                       &Globals, &Undefs);
  EXPECT_EQ(S0.size(), 0U);
  ASSERT_EQ(Globals.size(), 1U);
  EXPECT_FALSE(this->getSymbol(Globals, 0U).hasDefinition());

  // Check that the undef list contains this symbol.
  {
    auto UndefsPos = Undefs.begin();
    ASSERT_NE(UndefsPos, Undefs.end());
    auto StPos = Globals.begin();
    EXPECT_EQ(&*UndefsPos, &*StPos);
  }

  // Create a definition of "f".
  constexpr auto InputNo = 1U;
  std::shared_ptr<pstore::repo::compilation const> C1 = this->compile(Name, Linkage);
  llvm::Optional<rld::LocalSymbolsContainer> S1 = Resolver_.defineSymbols(
      &Globals, &Undefs, *C1, InputNo, std::cref(ErrorCallback));
  ASSERT_TRUE(S1.hasValue());

  // Check that the undef list is now empty again.
  ASSERT_TRUE(Undefs.empty());

  // Make sure that we kept the definition from C1.
  {
    ASSERT_EQ(Globals.size(), 1U);
    rld::Symbol const &Symbol0 = this->getSymbol(Globals, 0U);
    {
      SCOPED_TRACE("RefBeforeDef, DefinitionReplacesReference");
      this->checkSymbol(Symbol0, InputNo, Linkage, 0U);
      this->checkCompilationLocalView(*S1, Symbol0);
    }
    EXPECT_EQ(rld::referenceSymbol(Ctx_, getStringAddress(this->Db(), Name),
                                   *S1, &Globals, &Undefs),
              &Symbol0);
  }
}

INSTANTIATE_TEST_CASE_P(
    RefBeforeDef, RefBeforeDef,
    testing::Values(linkage::append, linkage::common, linkage::external,
                    linkage::link_once_odr, linkage::link_once_any,
                    linkage::weak_odr, linkage::weak_any), );

namespace {

//*  ___     _                     _    ___     _ _ _    _           *
//* |_ _|_ _| |_ ___ _ _ _ _  __ _| |  / __|___| | (_)__(_)___ _ _   *
//*  | || ' \  _/ -_) '_| ' \/ _` | | | (__/ _ \ | | (_-< / _ \ ' \  *
//* |___|_||_\__\___|_| |_||_\__,_|_|  \___\___/_|_|_/__/_\___/_||_| *
//*                                                                  *

// If two CUs define a symbol with the same name but one of them has internal
// linkage, then the internal symbol is only visible inside its defining CU.
class InternalCollision : public TwoSymbols,
                          public testing::WithParamInterface<linkage> {
protected:
  static constexpr auto Name_ = "f";
};

} // end anonymous namespace

// Defines the same name in two different CUs: one OtherLinkage and one internal
// (in that order).
TEST_P(InternalCollision, InternalAfter) {
  linkage const OtherLinkage = this->GetParam();

  ErrorFn ErrorCallback;
  EXPECT_CALL(ErrorCallback, invoke(_)).Times(0);

  rld::UndefsContainer Undefs;
  rld::GlobalSymbolsContainer Globals;

  ReturnType C0 = Resolver_.defineSymbols(&Globals, &Undefs,
                                          *this->compile(Name_, OtherLinkage),
                                          Input0_, std::cref(ErrorCallback));
  ASSERT_TRUE(C0.hasValue());
  ReturnType C1 = Resolver_.defineSymbols(
      &Globals, &Undefs, *this->compile(Name_, linkage::internal), Input1_,
      std::cref(ErrorCallback));
  ASSERT_TRUE(C1.hasValue());

  ASSERT_EQ(Globals.size(), 2U);
  EXPECT_TRUE(Undefs.empty());
  rld::Symbol const &Symbol0 = this->getSymbol(Globals, 0U);
  rld::Symbol const &Symbol1 = this->getSymbol(Globals, 1U);
  {
    SCOPED_TRACE("InternalCollision, InternalAfter");
    this->checkSymbol(Symbol0, Input0_, OtherLinkage, 0U);
    this->checkSymbol(Symbol1, Input1_, linkage::internal, 1U);
    this->checkCompilationLocalView(*C0, Symbol0);
    this->checkCompilationLocalView(*C1, Symbol1);
  }
  EXPECT_EQ(rld::referenceSymbol(Ctx_, getStringAddress(this->Db(), Name_), *C0,
                                 &Globals, &Undefs),
            &Symbol0);
  EXPECT_EQ(rld::referenceSymbol(Ctx_, getStringAddress(this->Db(), Name_), *C1,
                                 &Globals, &Undefs),
            &Symbol1);
  EXPECT_TRUE(Undefs.empty());
}

// Defines the same name in two different CUs: one internal and one OtherLinkage
// (in that order).
TEST_P(InternalCollision, InternalBefore) {
  linkage const OtherLinkage = this->GetParam();

  ErrorFn ErrorCallback;
  EXPECT_CALL(ErrorCallback, invoke(testing::_)).Times(0);

  rld::UndefsContainer Undefs;
  rld::GlobalSymbolsContainer Globals;

  ReturnType C0 = Resolver_.defineSymbols(
      &Globals, &Undefs, *this->compile(Name_, linkage::internal), Input0_,
      std::cref(ErrorCallback));
  ASSERT_TRUE(C0.hasValue());
  ReturnType C1 = Resolver_.defineSymbols(&Globals, &Undefs,
                                          *this->compile(Name_, OtherLinkage),
                                          Input1_, std::cref(ErrorCallback));
  ASSERT_TRUE(C1.hasValue());

  // Check that the resolver did the right thing. First that the symbol table is
  // as expected.
  EXPECT_TRUE(Undefs.empty());
  rld::Symbol const &Symbol0 = this->getSymbol(Globals, 0U);
  rld::Symbol const &Symbol1 = this->getSymbol(Globals, 1U);
  {
    SCOPED_TRACE("InternalCollision, InternalBefore");
    this->checkSymbol(Symbol0, Input0_, linkage::internal, 0U);
    this->checkSymbol(Symbol1, Input1_, OtherLinkage, 1U);
    this->checkCompilationLocalView(*C0, Symbol0);
    this->checkCompilationLocalView(*C1, Symbol1);
  }
  EXPECT_EQ(rld::referenceSymbol(Ctx_, getStringAddress(this->Db(), Name_), *C0,
                                 &Globals, &Undefs),
            &Symbol0);
  EXPECT_EQ(rld::referenceSymbol(Ctx_, getStringAddress(this->Db(), Name_), *C1,
                                 &Globals, &Undefs),
            &Symbol1);
}

INSTANTIATE_TEST_CASE_P(InternalWithCollision, InternalCollision,
                        testing::Values(linkage::append, linkage::common,
                                        linkage::external, linkage::internal,
                                        linkage::link_once_any,
                                        linkage::link_once_odr,
                                        linkage::weak_any,
                                        linkage::weak_odr), );
