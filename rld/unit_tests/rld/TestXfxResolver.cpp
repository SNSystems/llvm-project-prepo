//*  _____         _  __  __ __      ____                 _                 *
//* |_   _|__  ___| |_\ \/ // _|_  _|  _ \ ___  ___  ___ | |_   _____ _ __  *
//*   | |/ _ \/ __| __|\  /| |_\ \/ / |_) / _ \/ __|/ _ \| \ \ / / _ \ '__| *
//*   | |  __/\__ \ |_ /  \|  _|>  <|  _ <  __/\__ \ (_) | |\ V /  __/ |    *
//*   |_|\___||___/\__/_/\_\_| /_/\_\_| \_\___||___/\___/|_| \_/ \___|_|    *
//*                                                                         *
//===- unit_tests/rld/TestXfxResolver.cpp ---------------------------------===//
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
#include "rld/XfxScanner.h"

#include "gmock/gmock.h"

// Local includes
#include "CompilationBuilder.h"
#include "EmptyStore.h"
#include "ErrorFn.h"

using pstore::repo::linkage;
using testing::_;
using testing::UnorderedElementsAre;

namespace {

// create fragment with reference to
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
static pstore::index::fragment_index::value_type
createFragmentWithReferenceTo(CompilationBuilder::Transaction &T,
                              StringAdder &Strings,
                              llvm::StringRef const &Name) {
  using pstore::repo::fragment;
  using pstore::repo::generic_section_creation_dispatcher;
  using pstore::repo::section_content;
  using pstore::repo::section_kind;

  section_content DataSection{section_kind::read_only,
                              uint8_t{1} /*alignment*/};
  DataSection.data.emplace_back(uint8_t{0});
  DataSection.xfixups.emplace_back(Strings.add(T, Name), // name
                                   0,                    // relocation_type
                                   0,                    // offset
                                   0);                   // addend

  std::array<generic_section_creation_dispatcher, 1> Dispatchers{
      {{section_kind::read_only, &DataSection}}};

  auto FragmentIndex = CompilationBuilder::getFragmentIndex(T.db());
  return *FragmentIndex
              ->insert(T, std::make_pair(
                              pstore::index::digest{FragmentIndex->size()},
                              fragment::alloc(T, std::begin(Dispatchers),
                                              std::end(Dispatchers))))
              .first;
}

class XfxScannerTest : public testing::Test, public EmptyStore {
public:
  XfxScannerTest() : Context_{this->Db()}, CompilationBuilder_{this->Db()} {}

protected:
  using CompilationPtr = std::shared_ptr<pstore::repo::compilation const>;

  CompilationPtr
  compileOneDefinitionWithReferenceTo(llvm::StringRef const &Name,
                                      linkage Linkage,
                                      llvm::StringRef const &RefTo);

  llvm::Optional<rld::LocalSymbolsContainer>
  defineSymbols(CompilationPtr const &Compilation, uint32_t InputOrdinal);

  rld::Context Context_;
  rld::UndefsContainer Undefs_;
  rld::GlobalSymbolsContainer Globals_;

  CompilationBuilder CompilationBuilder_;
};

// compile one definition with reference to
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
auto XfxScannerTest::compileOneDefinitionWithReferenceTo(
    llvm::StringRef const &Name, linkage Linkage, llvm::StringRef const &RefTo)
    -> CompilationPtr {
  std::array<CompilationBuilder::NameAndLinkagePair, 1> NL{{{std::string(Name), Linkage}}};
  return CompilationBuilder_.compile(
      std::begin(NL), std::end(NL),
      [&RefTo](CompilationBuilder::Transaction &T, StringAdder &Strings) {
        return createFragmentWithReferenceTo(T, Strings, RefTo);
      });
}

// define symbols
// ~~~~~~~~~~~~~~
llvm::Optional<rld::LocalSymbolsContainer>
XfxScannerTest::defineSymbols(CompilationPtr const &Compilation,
                              uint32_t InputOrdinal) {
  // Create an entry in the symbol table for the definition in our compilation.
  ErrorFn ErrorCallback;
  EXPECT_CALL(ErrorCallback, invoke(_)).Times(0);
  rld::SymbolResolver Resolver{Context_};
  return Resolver.defineSymbols(&Globals_, &Undefs_, *Compilation, InputOrdinal,
                                std::cref(ErrorCallback));
}

} // end anonymous namespace

TEST_F(XfxScannerTest, Empty) {
  rld::LocalSymbolsContainer Locals;
  constexpr auto InputOrdinal = uint32_t{0};
  bool Ok = resolveXfixups(Context_, Locals, &Globals_, &Undefs_, InputOrdinal);
  EXPECT_TRUE(Ok);
  EXPECT_TRUE(Undefs_.empty());
  EXPECT_TRUE(Locals.empty());
  EXPECT_TRUE(Globals_.empty());
}

TEST_F(XfxScannerTest, RefToUndef) {
  constexpr auto InputOrdinal = uint32_t{7};

  // Create a compilation containing a single symbol ("f") with external
  // linkage. The sole fragment contains an external fixup referencing undefined
  // symbol "x".
  auto const Compilation =
      this->compileOneDefinitionWithReferenceTo("f", linkage::external, "x");

  // Create an entry in the symbol table for the definition in our compilation.
  auto const Locals = this->defineSymbols(Compilation, InputOrdinal);
  ASSERT_TRUE(Locals.hasValue()) << "Expected defineSymbols to succeed";

  // Resolve the external fixups in our compilation.
  bool Ok =
      resolveXfixups(Context_, *Locals, &Globals_, &Undefs_, InputOrdinal);
  EXPECT_TRUE(Ok);

  EXPECT_EQ(Undefs_.size(), 1U) << "There should be 1 undefined symbol";
  EXPECT_EQ(Globals_.size(), 2U)
      << "Expected 2 globals: the compilation member and the undef";
  EXPECT_EQ(Locals->size(), 1U)
      << "The compilation should have a single definition";

  // Build an intermediate vector with just the name and has-definition values.
  // This avoids baking the order of the global symbol table into the test.
  std::vector<std::pair<rld::StringAddress, bool>> G;
  std::transform(std::begin(Globals_), std::end(Globals_),
                 std::back_inserter(G), [](rld::Symbol const &Sym) {
                   return std::make_pair(Sym.name(), Sym.hasDefinition());
                 });
  EXPECT_THAT(G,
              UnorderedElementsAre(
                  std::make_pair(CompilationBuilder_.storeString("f"), true),
                  std::make_pair(CompilationBuilder_.storeString("x"), false)));
}

TEST_F(XfxScannerTest, RefToExternalDef) {
  constexpr auto InputOrdinal = uint32_t{11};

  // Create a compilation containing a single symbol ("f") with external
  // linkage. The sole fragment contains an external fixup referencing "f".
  auto const Compilation =
      this->compileOneDefinitionWithReferenceTo("f", linkage::external, "f");

  // Create an entry in the symbol table for the definition in our compilation.
  auto const Locals = this->defineSymbols(Compilation, InputOrdinal);
  ASSERT_TRUE(Locals.hasValue()) << "Expected defineSymbols to succeed";

  // Resolve the external fixups in our compilation.
  bool Ok =
      resolveXfixups(Context_, *Locals, &Globals_, &Undefs_, InputOrdinal);
  EXPECT_TRUE(Ok);

  EXPECT_TRUE(Undefs_.empty());
  EXPECT_EQ(Globals_.size(), 1U);
  EXPECT_EQ(Locals->size(), 1U)
      << "The compilation should have a single definition";

  auto const Pos = std::begin(Globals_);
  EXPECT_EQ(Pos->name(), CompilationBuilder_.storeString("f"));
  EXPECT_TRUE(Pos->hasDefinition()) << "Expected 'f' to be defined";
}

TEST_F(XfxScannerTest, RefToAppendDef) {
  constexpr auto InputOrdinal0 = uint32_t{13};
  constexpr auto InputOrdinal1 = uint32_t{17};

  // Create two compilations each containing a definition ("f") with append
  // linkage. Each contains a single fragment which contains an external fixup
  // referencing "f".
  auto const C0 =
      this->compileOneDefinitionWithReferenceTo("f", linkage::append, "f");
  auto const C1 =
      this->compileOneDefinitionWithReferenceTo("f", linkage::append, "f");

  auto const L0 = this->defineSymbols(C0, InputOrdinal0);
  ASSERT_TRUE(L0.hasValue()) << "Expected defineSymbols for C0 to succeed";
  EXPECT_TRUE(
      resolveXfixups(Context_, *L0, &Globals_, &Undefs_, InputOrdinal0));

  auto const L1 = this->defineSymbols(C1, InputOrdinal1);
  ASSERT_TRUE(L1.hasValue()) << "Expected defineSymbols for C1 to succeed";
  EXPECT_TRUE(
      resolveXfixups(Context_, *L1, &Globals_, &Undefs_, InputOrdinal1));

  EXPECT_TRUE(Undefs_.empty());
  EXPECT_EQ(Globals_.size(), 1U);
  EXPECT_EQ(L0->size(), 1U)
      << "Locals for Compilation 0 should have a single definition";
  EXPECT_EQ(L1->size(), 1U)
      << "Locals for Compilation 1 should have a single definition";

  auto const Pos = std::begin(Globals_);
  EXPECT_EQ(Pos->name(), CompilationBuilder_.storeString("f"));
  EXPECT_TRUE(Pos->hasDefinition()) << "Expected 'f' to be defined";
}
