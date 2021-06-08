//===- unit_tests/rld/TestXfxResolver.cpp ---------------------------------===//
//* __  __ __        ____                 _                 *
//* \ \/ // _|_  __ |  _ \ ___  ___  ___ | |_   _____ _ __  *
//*  \  /| |_\ \/ / | |_) / _ \/ __|/ _ \| \ \ / / _ \ '__| *
//*  /  \|  _|>  <  |  _ <  __/\__ \ (_) | |\ V /  __/ |    *
//* /_/\_\_| /_/\_\ |_| \_\___||___/\___/|_| \_/ \___|_|    *
//*                                                         *
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#include "rld/XfxScanner.h"

#include "gmock/gmock.h"

// Local includes
#include "CompilationBuilder.h"
#include "EmptyStore.h"
#include "ErrorFn.h"

using pstore::repo::linkage;
using pstore::repo::reference_strength;
using testing::_;
using testing::UnorderedElementsAre;

TEST(ReserveContiguous, TwoElementsTwice) {
  pstore::chunked_vector<int *, 3> cv;
  int a = 3;
  int b = 5;
  int c = 7;
  int d = 11;
  {
    int **p1 = rld::reserveContiguous(&cv, 2);
    *p1 = &a;
    ++p1;
    *p1 = &b;
  }
  {
    int **p2 = rld::reserveContiguous(&cv, 2);
    *p2 = &c;
    ++p2;
    *p2 = &d;
  }
  EXPECT_EQ(cv.size(), 5U);
  auto it = cv.begin();
  // chunk 0
  EXPECT_EQ(*it, &a);
  std::advance(it, 1);
  EXPECT_EQ(*it, &b);
  std::advance(it, 1);
  EXPECT_EQ(*it, nullptr);
  // chunk 1
  std::advance(it, 1);
  EXPECT_EQ(*it, &c);
  std::advance(it, 1);
  EXPECT_EQ(*it, &d);

  std::advance(it, 1);
  EXPECT_EQ(it, cv.end());
}

TEST(ReserveContiguous, OneElementThenThree) {
  pstore::chunked_vector<int *, 3> cv;
  int a = 3;
  int b = 5;
  int c = 7;
  int d = 11;
  {
    int **p1 = rld::reserveContiguous(&cv, 1);
    *p1 = &a;
  }
  {
    // A request for three contiguous elements when the container. These should
    // occupy the second chunk, with the remaining unoccupied elements of the
    // first chunk being filled with nullptr.
    int **p2 = rld::reserveContiguous(&cv, 3);
    *p2 = &b;
    ++p2;
    *p2 = &c;
    ++p2;
    *p2 = &d;
  }

  EXPECT_EQ(cv.size(), 6U);
  auto it = cv.begin();
  // chunk 0.
  EXPECT_EQ(*it, &a);
  std::advance(it, 1);
  EXPECT_EQ(*it, nullptr);
  std::advance(it, 1);
  EXPECT_EQ(*it, nullptr);
  std::advance(it, 1);
  // chunk 1.
  EXPECT_EQ(*it, &b);
  std::advance(it, 1);
  EXPECT_EQ(*it, &c);
  std::advance(it, 1);
  EXPECT_EQ(*it, &d);

  std::advance(it, 1);
  EXPECT_EQ(it, cv.end());
}

// create fragment with reference to
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
static pstore::index::fragment_index::value_type
createFragmentWithReferenceTo(CompilationBuilder::Transaction &T,
                              StringAdder &Strings, llvm::StringRef const &Name,
                              reference_strength Strength) {
  using pstore::repo::fragment;
  using pstore::repo::generic_section_creation_dispatcher;
  using pstore::repo::section_content;
  using pstore::repo::section_kind;

  section_content DataSection{section_kind::read_only,
                              UINT8_C(1) /*alignment*/};
  DataSection.data.emplace_back(UINT8_C(0));
  DataSection.xfixups.emplace_back(Strings.add(T, Name),
                                   pstore::repo::relocation_type{0}, Strength,
                                   0U, // offset
                                   0); // addend

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

namespace {

class XfxScannerTest : public testing::Test, public EmptyStore {
public:
  XfxScannerTest()
      : Context_{this->Db(), "_start"}, CompilationBuilder_{this->Db()} {}

protected:
  using CompilationPtr = std::shared_ptr<pstore::repo::compilation const>;

  CompilationPtr compileOneDefinitionWithReferenceTo(
      llvm::StringRef const &Name, linkage Linkage,
      llvm::StringRef const &RefTo, reference_strength Strength);

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
    llvm::StringRef const &Name, linkage Linkage, llvm::StringRef const &RefTo,
    reference_strength Strength) -> CompilationPtr {
  std::array<CompilationBuilder::NameAndLinkagePair, 1> NL{{{std::string(Name), Linkage}}};
  return CompilationBuilder_.compile(
      std::begin(NL), std::end(NL),
      [&RefTo, Strength](CompilationBuilder::Transaction &T,
                         StringAdder &Strings) {
        return createFragmentWithReferenceTo(T, Strings, RefTo, Strength);
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
  pstore::chunked_vector<rld::Symbol *> ResolvedFixups;
  constexpr auto InputOrdinal = uint32_t{0};
  const rld::LocalPLTsContainer PLTSymbols = resolveXfixups(
      Context_, Locals, &Globals_, &Undefs_, &ResolvedFixups, InputOrdinal);
  EXPECT_TRUE(PLTSymbols.empty());
  EXPECT_TRUE(Undefs_.empty());
  EXPECT_EQ(Undefs_.strongUndefCount(), 0U);
  EXPECT_TRUE(Undefs_.strongUndefCountIsCorrect());
  EXPECT_TRUE(Locals.empty());
  EXPECT_TRUE(Globals_.empty());
}

TEST_F(XfxScannerTest, StrongRefToUndefined) {
  constexpr auto InputOrdinal = uint32_t{7};

  // Create a compilation containing a single symbol ("f") with external
  // linkage. The sole fragment contains an external fixup with a strong
  // reference to undefined symbol "x".
  auto const Compilation = this->compileOneDefinitionWithReferenceTo(
      "f", linkage::external, "x", reference_strength::strong);

  // Create an entry in the symbol table for the definition in our compilation.
  auto const Locals = this->defineSymbols(Compilation, InputOrdinal);
  ASSERT_TRUE(Locals.hasValue()) << "Expected defineSymbols to succeed";

  // Resolve the external fixups in our compilation.
  pstore::chunked_vector<rld::Symbol *> ResolvedFixups;
  const rld::LocalPLTsContainer PLTSymbols = resolveXfixups(
      Context_, *Locals, &Globals_, &Undefs_, &ResolvedFixups, InputOrdinal);
  EXPECT_TRUE(PLTSymbols.empty());

  EXPECT_EQ(Undefs_.size(), 1U) << "There should be 1 undefined symbol";
  EXPECT_EQ(Undefs_.strongUndefCount(), 1U)
      << "There should be 1 strong-undefined symbol";
  EXPECT_TRUE(Undefs_.strongUndefCountIsCorrect());
  EXPECT_EQ(Globals_.size(), 2U)
      << "Expected 2 globals: the definition and the undef";
  EXPECT_EQ(Locals->size(), 1U)
      << "The compilation should have a single definition";

  // Build an intermediate vector with just the name and has-definition values.
  // This avoids baking the order of the global symbol table into the test.
  std::vector<std::pair<pstore::address, bool>> G;
  std::transform(std::begin(Globals_), std::end(Globals_),
                 std::back_inserter(G), [](rld::Symbol const &Sym) {
                   return std::make_pair(Sym.name(), Sym.hasDefinition());
                 });
  EXPECT_THAT(G, UnorderedElementsAre(
                     std::make_pair(CompilationBuilder_.directStringAddress(
                                        CompilationBuilder_.storeString("f")),
                                    true),
                     std::make_pair(CompilationBuilder_.directStringAddress(
                                        CompilationBuilder_.storeString("x")),
                                    false)));
}

TEST_F(XfxScannerTest, WeakRefToUndefined) {
  constexpr auto InputOrdinal = uint32_t{7};

  // Create a compilation containing a single symbol ("f") with external
  // linkage. The sole fragment contains an external fixup with a weak reference
  // to undefined symbol "x".
  auto const Compilation = this->compileOneDefinitionWithReferenceTo(
      "f", linkage::external, "x", reference_strength::weak);

  // Create an entry in the symbol table for the definition in our compilation.
  auto const Locals = this->defineSymbols(Compilation, InputOrdinal);
  ASSERT_TRUE(Locals.hasValue()) << "Expected defineSymbols to succeed";

  // Resolve the external fixups in our compilation.
  pstore::chunked_vector<rld::Symbol *> ResolvedFixups;
  const rld::LocalPLTsContainer PLTSymbols = resolveXfixups(
      Context_, *Locals, &Globals_, &Undefs_, &ResolvedFixups, InputOrdinal);
  EXPECT_TRUE(PLTSymbols.empty());

  EXPECT_EQ(Undefs_.size(), 1U) << "There should be 1 undefined symbol";
  EXPECT_EQ(Undefs_.strongUndefCount(), 0U)
      << "There should be no strong-undefined symbols";
  EXPECT_TRUE(Undefs_.strongUndefCountIsCorrect());
  EXPECT_EQ(Globals_.size(), 2U)
      << "Expected 2 globals: the definition and the undef";
  EXPECT_EQ(Locals->size(), 1U)
      << "The compilation should have a single definition";

  // Build an intermediate vector with just the name and has-definition values.
  // This avoids baking the order of the global symbol table into the test.
  std::vector<std::pair<pstore::address, bool>> G;
  std::transform(std::begin(Globals_), std::end(Globals_),
                 std::back_inserter(G), [](rld::Symbol const &Sym) {
                   return std::make_pair(Sym.name(), Sym.hasDefinition());
                 });
  EXPECT_THAT(G, UnorderedElementsAre(
                     std::make_pair(CompilationBuilder_.directStringAddress(
                                        CompilationBuilder_.storeString("f")),
                                    true),
                     std::make_pair(CompilationBuilder_.directStringAddress(
                                        CompilationBuilder_.storeString("x")),
                                    false)));
}

// Checks that a weakly referenced undef is promoted to a strongly-referenced
// undef when a strong reference is encountered.
TEST_F(XfxScannerTest, WeakThenStrongRefToUndef) {
  constexpr auto InputOrdinal1 = uint32_t{7};
  constexpr auto InputOrdinal2 = InputOrdinal1 + 1U;
  pstore::chunked_vector<rld::Symbol *> ResolvedFixups;
  {
    // Create a compilation containing a single symbol ("f") with external
    // linkage. The sole fragment contains an external fixup with a weak
    // reference to the undefined symbol "x".
    auto const Compilation1 = this->compileOneDefinitionWithReferenceTo(
        "f", linkage::external, "x", reference_strength::weak);
    // Create an entry in the symbol table for the definition in our
    // compilation.
    auto const Locals1 = this->defineSymbols(Compilation1, InputOrdinal1);
    ASSERT_TRUE(Locals1.hasValue()) << "Expected defineSymbols to succeed";
    EXPECT_EQ(Locals1->size(), 1U);
    // Resolve the external fixups in compilation #1.
    const rld::LocalPLTsContainer PLTSymbols1 =
        resolveXfixups(Context_, *Locals1, &Globals_, &Undefs_, &ResolvedFixups,
                       InputOrdinal1);
    EXPECT_TRUE(PLTSymbols1.empty());
  }

  // Check the state after the first compilation has been processed.
  EXPECT_EQ(Undefs_.size(), 1U) << "There should be 1 undefined symbol";
  EXPECT_EQ(Undefs_.strongUndefCount(), 0U)
      << "There should be no strong-undefined symbols";
  EXPECT_TRUE(Undefs_.strongUndefCountIsCorrect());
  EXPECT_EQ(Globals_.size(), 2U)
      << "Expected 2 globals: the definition (f) and the undef (x)";

  {
    // Create a compilation containing a single symbol ("g") with external
    // linkage. The sole fragment contains an external fixup with a strong
    // reference to the undefined symbol "x".
    auto const Compilation2 = this->compileOneDefinitionWithReferenceTo(
        "g", linkage::external, "x", reference_strength::strong);
    // Create an entry in the symbol table for the definition in our
    // compilation.
    auto const Locals2 = this->defineSymbols(Compilation2, InputOrdinal2);
    ASSERT_TRUE(Locals2.hasValue()) << "Expected defineSymbols to succeed";
    EXPECT_EQ(Locals2->size(), 1U);
    // Resolve the external fixups in compilation #2.
    const rld::LocalPLTsContainer PLTSymbols2 =
        resolveXfixups(Context_, *Locals2, &Globals_, &Undefs_, &ResolvedFixups,
                       InputOrdinal2);
    EXPECT_TRUE(PLTSymbols2.empty());
  }

  EXPECT_EQ(Undefs_.size(), 1U) << "There should be 1 undefined symbol";
  EXPECT_EQ(Undefs_.strongUndefCount(), 1U)
      << "There should be 1 strong-undefined symbol";
  EXPECT_TRUE(Undefs_.strongUndefCountIsCorrect());
  EXPECT_EQ(Globals_.size(), 3U)
      << "Expected 3 globals: the two definitions (f,g) and the undef (x)";
}

TEST_F(XfxScannerTest, StrongRefToExternalDef) {
  constexpr auto InputOrdinal = uint32_t{11};
  pstore::chunked_vector<rld::Symbol *> ResolvedFixups;

  // Create a compilation containing a single symbol ("f") with external
  // linkage. The sole fragment contains an external fixup referencing "f".
  auto const Compilation = this->compileOneDefinitionWithReferenceTo(
      "f", linkage::external, "f", reference_strength::strong);

  // Create an entry in the symbol table for the definition in our compilation.
  auto const Locals = this->defineSymbols(Compilation, InputOrdinal);
  ASSERT_TRUE(Locals.hasValue()) << "Expected defineSymbols to succeed";

  // Resolve the external fixups in our compilation.
  const rld::LocalPLTsContainer PLTSymbols = resolveXfixups(
      Context_, *Locals, &Globals_, &Undefs_, &ResolvedFixups, InputOrdinal);
  EXPECT_TRUE(PLTSymbols.empty());

  EXPECT_TRUE(Undefs_.empty());
  EXPECT_EQ(Undefs_.strongUndefCount(), 0U);
  EXPECT_TRUE(Undefs_.strongUndefCountIsCorrect());
  EXPECT_EQ(Globals_.size(), 1U);
  EXPECT_EQ(Locals->size(), 1U)
      << "The compilation should have a single definition";

  auto const Pos = std::begin(Globals_);
  EXPECT_EQ(Pos->name(), CompilationBuilder_.directStringAddress(
                             CompilationBuilder_.storeString("f")));
  EXPECT_TRUE(Pos->hasDefinition()) << "Expected 'f' to be defined";
}

TEST_F(XfxScannerTest, RefToAppendDef) {
  constexpr auto InputOrdinal0 = uint32_t{13};
  constexpr auto InputOrdinal1 = uint32_t{17};
  pstore::chunked_vector<rld::Symbol *> ResolvedFixups;

  // Create two compilations each containing a definition ("f") with append
  // linkage. Each contains a single fragment which contains an external fixup
  // referencing "f".
  auto const C0 = this->compileOneDefinitionWithReferenceTo(
      "f", linkage::append, "f", reference_strength::strong);
  auto const C1 = this->compileOneDefinitionWithReferenceTo(
      "f", linkage::append, "f", reference_strength::strong);

  auto const L0 = this->defineSymbols(C0, InputOrdinal0);
  ASSERT_TRUE(L0.hasValue()) << "Expected defineSymbols for C0 to succeed";
  const rld::LocalPLTsContainer PLTSymbols0 = resolveXfixups(
      Context_, *L0, &Globals_, &Undefs_, &ResolvedFixups, InputOrdinal0);
  EXPECT_TRUE(PLTSymbols0.empty());

  auto const L1 = this->defineSymbols(C1, InputOrdinal1);
  ASSERT_TRUE(L1.hasValue()) << "Expected defineSymbols for C1 to succeed";
  const rld::LocalPLTsContainer PLTSymbols1 = resolveXfixups(
      Context_, *L1, &Globals_, &Undefs_, &ResolvedFixups, InputOrdinal1);
  EXPECT_TRUE(PLTSymbols1.empty());

  EXPECT_TRUE(Undefs_.empty());
  EXPECT_EQ(Undefs_.strongUndefCount(), 0U);
  EXPECT_TRUE(Undefs_.strongUndefCountIsCorrect());
  EXPECT_EQ(Globals_.size(), 1U);
  EXPECT_EQ(L0->size(), 1U)
      << "Locals for Compilation 0 should have a single definition";
  EXPECT_EQ(L1->size(), 1U)
      << "Locals for Compilation 1 should have a single definition";

  auto const Pos = std::begin(Globals_);
  EXPECT_EQ(Pos->name(), CompilationBuilder_.directStringAddress(
                             CompilationBuilder_.storeString("f")));
  EXPECT_TRUE(Pos->hasDefinition()) << "Expected 'f' to be defined";
}
