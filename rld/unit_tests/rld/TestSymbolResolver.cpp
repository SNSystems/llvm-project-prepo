#include "rld/symbol.h"

#include "EmptyStore.h"

#include "pstore/core/hamt_set.hpp"
#include "pstore/os/memory_mapper.hpp"

#include "rld/context.h"

#include "gmock/gmock.h"

#include <array>
#include <cassert>
#include <iterator>

using pstore::repo::linkage;

namespace {

// FIXME: copied from gen.cpp
class StringAdder {
public:
  explicit StringAdder(
      std::shared_ptr<pstore::index::name_index> const NamesIndex)
      : NamesIndex_{std::move(NamesIndex)} {}

  template <typename Lock>
  rld::StringAddress add(pstore::transaction<Lock> &Transaction,
                         std::string const &Str) {
    Strings_.emplace_back(Str, pstore::raw_sstring_view{});
    auto &Back = Strings_.back();
    Back.second =
        pstore::raw_sstring_view{Back.first.data(), Back.first.size()};
    auto const AddRes = Adder_.add(Transaction, NamesIndex_, &Back.second);
    if (!AddRes.second) {
      // FIXME: already there! Delete the last entry
    }
    return rld::StringAddress::make(AddRes.first.get_address());
  }

  template <typename Lock> void flush(pstore::transaction<Lock> &Transaction) {
    Adder_.flush(Transaction);
  }

private:
  std::shared_ptr<pstore::index::name_index> NamesIndex_;
  std::list<std::pair<std::string, pstore::raw_sstring_view>> Strings_;
  pstore::indirect_string_adder Adder_;
};


class ErrorFnInterface {
public:
  virtual ~ErrorFnInterface() = default;
  virtual void invoke(rld::StringAddress) const = 0;
  void operator()(rld::StringAddress Addr) const { return invoke(Addr); }
};
class ErrorFn : public ErrorFnInterface {
public:
  MOCK_CONST_METHOD1(invoke, void(rld::StringAddress));
};

//*  ___            _         _ ___                             *
//* / __|_  _ _ __ | |__  ___| / __| __ __ _ _ _  _ _  ___ _ _  *
//* \__ \ || | '  \| '_ \/ _ \ \__ \/ _/ _` | ' \| ' \/ -_) '_| *
//* |___/\_, |_|_|_|_.__/\___/_|___/\__\__,_|_||_|_||_\___|_|   *
//*      |__/                                                   *
class SymbolScanner : public testing::Test, public EmptyStore {
public:
  SymbolScanner() = default;

protected:
  using Transaction = pstore::transaction<pstore::transaction_lock>;
  using NameAndLinkagePair = std::pair<std::string, linkage>;

  template <typename NameAndLinkageIterator, typename CreateFragmentFn>
  static std::pair<pstore::index::digest,
                   pstore::extent<pstore::repo::compilation>>
  createCompilation(Transaction &T, StringAdder &NameAdder,
                    NameAndLinkageIterator first, NameAndLinkageIterator last,
                    CreateFragmentFn FragmentCreator);

  static std::shared_ptr<pstore::index::fragment_index>
  getFragmentIndex(pstore::database &Db) {
    return pstore::index::get_index<pstore::trailer::indices::fragment>(Db);
  }

  static std::shared_ptr<pstore::index::compilation_index>
  getCompilationIndex(pstore::database &Db) {
    return pstore::index::get_index<pstore::trailer::indices::compilation>(Db);
  }

  static std::shared_ptr<pstore::index::name_index>
  getNameIndex(pstore::database &Db) {
    return pstore::index::get_index<pstore::trailer::indices::name>(Db);
  }

  static rld::StringAddress getStringAddress(pstore::database const &Db,
                                             char const *Name);

  static void checkFragmentContents(
      std::shared_ptr<pstore::repo::fragment const> const &Fragment,
      std::uint8_t Index);

  std::shared_ptr<pstore::repo::compilation const>
  compile(std::string const &Name, linkage Linkage);

  // Create a transaction which simply contains the supplied string if not
  // already in the names index.
  rld::StringAddress storeString(char const *Name);
};

// getStringAddress
// ~~~~~~~~~~~~~~~~
rld::StringAddress SymbolScanner::getStringAddress(pstore::database const &Db,
                                                   char const *Name) {
  auto NameIndex = pstore::index::get_index<pstore::trailer::indices::name>(Db);
  pstore::raw_sstring_view const str = pstore::make_sstring_view(Name);
  auto Pos = NameIndex->find(Db, pstore::indirect_string{Db, &str});
  assert(Pos != NameIndex->end(Db));
  return rld::StringAddress::make(Pos.get_address());
}

// createCompilation
// ~~~~~~~~~~~~~~~~~
/// \tparam NameAndLinkageIterator An iterator whose value-type must be
///     NameAndLinkagePair.
/// \tparam CreateFragmentFn  A function which is called to create a single
///     fragment. It should be compatible with the signature:
///     function<pair<digest, extent<fragment>>(Transaction&)>
/// \param T  An open transaction to which the compilation will be added.
/// \param NameAddr  A StringAdder instance which is responsible for
///     managing strings that are added to the transaction.
/// \param first  The beginning of an iterator range which produces
///     NameAndLinkagePair instances.
/// \param last  The end of an iterator range which produces
///     NameAndLinkagePair instances.
/// \param FragmentCreator  A function which will create a single fragment
///     with the desired contents.
/// \returns A pair containing the new fragment's digest and its extent.
template <typename NameAndLinkageIterator, typename CreateFragmentFn>
std::pair<pstore::index::digest, pstore::extent<pstore::repo::compilation>>
SymbolScanner::createCompilation(Transaction &T, StringAdder &NameAdder,
                                 NameAndLinkageIterator first,
                                 NameAndLinkageIterator last,
                                 CreateFragmentFn FragmentCreator) {
  static_assert(
      std::is_same<
          typename std::iterator_traits<NameAndLinkageIterator>::value_type,
          NameAndLinkagePair>::value,
      "Iterator must produce NameAndLinkagePair");

  auto FragmentIndex = getFragmentIndex(T.db());
  auto CompilationIndex = getCompilationIndex(T.db());

  using Digest = pstore::index::digest;
  using Compilation = pstore::repo::compilation;
  using CompilationMember = pstore::repo::compilation_member;

  std::vector<CompilationMember> CompilationSymbols;
  std::transform(
      first, last, std::back_inserter(CompilationSymbols),
      [&FragmentCreator, &T, &NameAdder](NameAndLinkagePair const &NL) {
        auto const FragmentDigestAndExtent = FragmentCreator(T);
        return CompilationMember{FragmentDigestAndExtent.first,
                                 FragmentDigestAndExtent.second,
                                 NameAdder.add(T, NL.first), NL.second};
      });

  constexpr auto Path = "/path";
  constexpr auto Triple = "machine-vendor-os";
  return *(CompilationIndex
               ->insert(T, std::make_pair(Digest{CompilationIndex->size()},
                                          Compilation::alloc(
                                              T, NameAdder.add(T, Path),
                                              NameAdder.add(T, Triple),
                                              std::begin(CompilationSymbols),
                                              std::end(CompilationSymbols))))
               .first);
}

// checkFragmentContents
// ~~~~~~~~~~~~~~~~~~~~~
void SymbolScanner::checkFragmentContents(
    std::shared_ptr<pstore::repo::fragment const> const &Fragment,
    std::uint8_t Index) {
  ASSERT_NE(Fragment.get(), nullptr);
  EXPECT_EQ(Fragment->size(), 1U);
  pstore::repo::generic_section const *const rodata =
      Fragment->atp<pstore::repo::section_kind::read_only>();
  ASSERT_NE(rodata, nullptr);
  EXPECT_THAT(rodata->payload(), testing::ElementsAre(Index))
      << "Expected fragment #" << unsigned{Index};
}

// compile
// ~~~~~~~
std::shared_ptr<pstore::repo::compilation const>
SymbolScanner::compile(std::string const &Name, linkage Linkage) {
  using namespace pstore::repo;

  auto Transaction = pstore::begin(this->Db());
  StringAdder NameAdder(getNameIndex(this->Db()));
  auto const CreateFragment = [](decltype(Transaction) &T)
      -> std::pair<pstore::index::digest,
                   pstore::extent<pstore::repo::fragment>> {
    auto FragmentIndex = getFragmentIndex(T.db());
    auto const NumFragments = FragmentIndex->size();

    section_content DataSection{section_kind::read_only,
                                std::uint8_t{1} /*alignment*/};
    assert(NumFragments <
           std::numeric_limits<decltype(DataSection.data)::value_type>::max());
    DataSection.data.push_back(NumFragments);

    std::array<generic_section_creation_dispatcher, 1> Dispatchers{
        {{section_kind::read_only, &DataSection}}};
    return *FragmentIndex
                ->insert(T, std::make_pair(
                                pstore::index::digest{NumFragments},
                                fragment::alloc(T, std::begin(Dispatchers),
                                                std::end(Dispatchers))))
                .first;
  };
  std::array<NameAndLinkagePair, 1> NameAndLinkage{{{Name, Linkage}}};
  auto const CompilationDigestAndExtent =
      createCompilation(Transaction, NameAdder, std::begin(NameAndLinkage),
                        std::end(NameAndLinkage), CreateFragment);
  NameAdder.flush(Transaction); // Write the bodies of the strings.
  Transaction.commit();
  return compilation::load(this->Db(), CompilationDigestAndExtent.second);
}

// storeString
// ~~~~~~~~~~~
rld::StringAddress SymbolScanner::storeString(char const *Name) {
  auto Transaction = pstore::begin(this->Db());
  StringAdder NameAdder(getNameIndex(this->Db()));
  rld::StringAddress const SAddr = NameAdder.add(Transaction, Name);
  NameAdder.flush(Transaction); // Write the bodies of the strings.
  Transaction.commit();
  return SAddr;
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
      std::size_t{43}; // (Using an unimportant small prime.)

  rld::UndefsContainer Undefs;
  // Simulate a CU containing a single symbol ("f0") of type 'Linkage'. Create a
  // symbol resolver and get it to examine our new compilation.
  rld::GlobalSymbolsContainer Globals;
  ReturnType const C0Locals =
      Resolver_.defineSymbols(Undefs, &Globals, *this->compile("f0", Linkage),
                              InputOrdinal, std::cref(ErrorCallback));

  ASSERT_TRUE(C0Locals.hasValue());
  EXPECT_TRUE(Undefs.empty());

  {
    ASSERT_EQ(Globals.size(), 1U);
    rld::Symbol const &Sym = *Globals.begin();
    ASSERT_TRUE(Sym.definition().hasValue());
    llvm::SmallVector<rld::Symbol::Body, 1> const &Definition =
        *Sym.definition();
    ASSERT_EQ(Definition.size(), 1U);
    EXPECT_EQ(Definition.front().inputOrdinal(), InputOrdinal);
    EXPECT_EQ(Definition.front().linkage(), Linkage);

    SCOPED_TRACE("SingleSymbol,SingleSymbol");
    this->checkFragmentContents(Definition.front().fragment(), std::uint8_t{0});
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
                   linkage Linkage, std::uint8_t FragmentIndex);
  rld::Symbol const &getSymbol(rld::GlobalSymbolsContainer const &Globals,
                               unsigned Index);

  static constexpr auto Input0_ = 0U;
  static constexpr auto Input1_ = 1U;

  void checkCompilationLocalView(
      llvm::Optional<rld::LocalSymbolsContainer> const &LocalView,
      rld::Symbol const &S) const;

  rld::Context Ctx_;
  rld::SymbolResolver Resolver_;
};

// getSymbol
// ~~~~~~~~~
rld::Symbol const &
TwoSymbols::getSymbol(rld::GlobalSymbolsContainer const &Globals,
                      unsigned Index) {
  // auto const &ST = Ctx_.symbolTable();
  assert(Index < Globals.size());
  auto It = std::begin(Globals);
  std::advance(It, Index);
  return *It;
}

// checkSymbol
// ~~~~~~~~~~~
void TwoSymbols::checkSymbol(rld::Symbol const &Symbol,
                             unsigned TicketFileOrdinal, linkage Linkage,
                             std::uint8_t FragmentIndex) {
  ASSERT_TRUE(Symbol.definition().hasValue());
  auto const &Definition = *Symbol.definition();
  ASSERT_EQ(Definition.size(), 1U) << "Symbol must have a single body";
  EXPECT_EQ(Definition.front().inputOrdinal(), TicketFileOrdinal);
  EXPECT_EQ(Definition.front().linkage(), Linkage);
  // Ensure that we have the expected fragment associated with this symbol. We
  // create fragments with a single section whose data payload is a single byte
  // derived from the fragment-index size at the time it was created.
  this->checkFragmentContents(Definition.front().fragment(), FragmentIndex);
}

// checkCompilationLocalView
// ~~~~~~~~~~~~~~~~~~~~~~~~~
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
  std::shared_ptr<pstore::repo::compilation const> const CU0_;
  std::shared_ptr<pstore::repo::compilation const> const CU1_;

  void checkSuccess(ReturnType const &C0, ReturnType const &C1,
                    rld::Symbol const &Symbol0) const;
};

// ctor
// ~~~~
TwoCompilations::TwoCompilations()
    : CU0_{this->compile("f", std::get<0>(GetParam()))},
      CU1_{this->compile("f", std::get<1>(GetParam()))} {}

// checkSuccess
// ~~~~~~~~~~~~
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
  ReturnType C0 = Resolver_.defineSymbols(Undefs, &Globals, *CU0_, Input0_,
                                          std::cref(ErrorCallback));
  ASSERT_TRUE(C0);
  ReturnType C1 = Resolver_.defineSymbols(Undefs, &Globals, *CU1_, Input1_,
                                          std::cref(ErrorCallback));
  ASSERT_TRUE(C1);

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
  ReturnType C1 = Resolver_.defineSymbols(Undefs, &Globals, *CU1_, Input1_,
                                          std::cref(ErrorCallback));
  ASSERT_TRUE(C1);
  ReturnType C0 = Resolver_.defineSymbols(Undefs, &Globals, *CU0_, Input0_,
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

  // Two compilations defining the name name: the first with a symbol linkage
  // from the first test parameter, the second with the second parameter symbol
  // linkage.
  ReturnType C0 = Resolver_.defineSymbols(Undefs, &Globals, *CU0_, Input0_,
                                          std::cref(ErrorCallback));
  ASSERT_TRUE(C0);
  ReturnType C1 = Resolver_.defineSymbols(Undefs, &Globals, *CU1_, Input1_,
                                          std::cref(ErrorCallback));
  ASSERT_TRUE(C1);

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

  // Two compilations defining the name name: the first with a symbol linkage
  // from the first test parameter, the second with the second parameter symbol
  // linkage.
  ReturnType C0 = Resolver_.defineSymbols(Undefs, &Globals, *CU0_, Input0_,
                                          std::cref(ErrorCallback));
  ASSERT_TRUE(C0);
  ReturnType C1 = Resolver_.defineSymbols(Undefs, &Globals, *CU1_, Input1_,
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

  // Two compilations defining the name name: the first with an symbol linkage
  // from the first test parameter, the second with the second parameter symbol
  // linkage.
  ReturnType C0 = Resolver_.defineSymbols(Undefs, &Globals, *CU0_, Input0_,
                                          std::cref(ErrorCallback));
  ReturnType C1 = Resolver_.defineSymbols(Undefs, &Globals, *CU1_, Input1_,
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

//*  _                         _    *
//* | |   __ _ _ _ __ _ ___ __| |_  *
//* | |__/ _` | '_/ _` / -_|_-<  _| *
//* |____\__,_|_| \__, \___/__/\__| *
//*               |___/             *
namespace {

class Largest : public TwoSymbols {
protected:
  //  using ReturnType = llvm::Optional<rld::LocalSymbolsContainer>;
  static constexpr auto OrdinalA_ = std::size_t{43};
  static constexpr auto OrdinalB_ = std::size_t{47};
  static constexpr auto Name_ = "f";

  std::shared_ptr<pstore::repo::compilation const>
  compileWithCommonSymbol(std::string const &Name, unsigned CommonSize);

  void checkCommonSymbol(rld::Symbol const &Sym, std::size_t ExpectedOrdinal,
                         std::size_t ExpectedSize);
};

// compileWithCommonSymbol
// ~~~~~~~~~~~~~~~~~~~~~~~
std::shared_ptr<pstore::repo::compilation const>
Largest::compileWithCommonSymbol(std::string const &Name, unsigned CommonSize) {
  using namespace pstore::repo;

  auto Transaction = pstore::begin(this->Db());
  StringAdder NameAdder(getNameIndex(this->Db()));
  auto const CreateFragment = [CommonSize](decltype(Transaction) &T)
      -> std::pair<pstore::index::digest,
                   pstore::extent<pstore::repo::fragment>> {
    auto FragmentIndex = getFragmentIndex(T.db());
    auto const NumFragments = FragmentIndex->size();

    section_content BSSSection{section_kind::bss,
                               std::uint8_t{1} /*alignment*/};
    BSSSection.data.resize(CommonSize); //, std::uint8_t{0});
    bss_section_creation_dispatcher CD{&BSSSection};
    return *FragmentIndex
                ->insert(T, std::make_pair(pstore::index::digest{NumFragments},
                                           fragment::alloc(T, &CD, &CD + 1)))
                .first;
  };
  std::array<NameAndLinkagePair, 1> NameAndLinkage{{{Name, linkage::common}}};
  auto const CompilationDigestAndExtent =
      createCompilation(Transaction, NameAdder, std::begin(NameAndLinkage),
                        std::end(NameAndLinkage), CreateFragment);
  NameAdder.flush(Transaction); // Write the bodies of the strings.
  Transaction.commit();
  return compilation::load(this->Db(), CompilationDigestAndExtent.second);
}

// checkCommonSymbol
// ~~~~~~~~~~~~~~~~~
void Largest::checkCommonSymbol(rld::Symbol const &Sym,
                                std::size_t ExpectedOrdinal,
                                std::size_t ExpectedSize) {
  llvm::Optional<llvm::SmallVector<rld::Symbol::Body, 1>> const &Definition =
      Sym.definition();
  ASSERT_TRUE(Definition.hasValue()) << "The symbol should be defined";
  ASSERT_EQ(Definition->size(), 1U)
      << "The must be a single definition of the symbol";
  auto const &Body = Definition->front();
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
  auto CompilationA = this->compileWithCommonSymbol(Name_, SizeA);
  auto CompilationB = this->compileWithCommonSymbol(Name_, SizeB);

  rld::UndefsContainer Undefs;
  rld::GlobalSymbolsContainer Globals;

  ReturnType CA = Resolver_.defineSymbols(Undefs, &Globals, *CompilationA,
                                          OrdinalA_, std::cref(ErrorCallback));
  ASSERT_TRUE(CA.hasValue())
      << "Symbol resolution for OrdinalA_ produced an error";
  EXPECT_EQ(Globals.size(), 1U)
      << "The global symbol table should hold 1 entry";

  ReturnType CB = Resolver_.defineSymbols(Undefs, &Globals, *CompilationB,
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
  auto CompilationA = this->compileWithCommonSymbol(Name_, SizeA);
  auto CompilationB = this->compileWithCommonSymbol(Name_, SizeB);
  rld::UndefsContainer Undefs;
  rld::GlobalSymbolsContainer Globals;

  ReturnType CA = Resolver_.defineSymbols(Undefs, &Globals, *CompilationA,
                                          OrdinalA_, std::cref(ErrorCallback));
  ASSERT_TRUE(CA.hasValue());
  ReturnType CB = Resolver_.defineSymbols(Undefs, &Globals, *CompilationB,
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
  auto CompilationA = this->compileWithCommonSymbol(Name_, Size);
  auto CompilationB = this->compileWithCommonSymbol(Name_, Size);
  rld::UndefsContainer Undefs;
  rld::GlobalSymbolsContainer Globals;
  ReturnType CA = Resolver_.defineSymbols(Undefs, &Globals, *CompilationA,
                                          OrdinalA_, std::cref(ErrorCallback));
  ASSERT_TRUE(CA.hasValue());
  ReturnType CB = Resolver_.defineSymbols(Undefs, &Globals, *CompilationB,
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
  auto CompilationA = this->compileWithCommonSymbol(Name_, Size);
  auto CompilationB = this->compileWithCommonSymbol(Name_, Size);
  rld::UndefsContainer Undefs;
  rld::GlobalSymbolsContainer Globals;
  // Define the symbol from the file with the higher ordinal _first_,
  ReturnType CB = Resolver_.defineSymbols(Undefs, &Globals, *CompilationA,
                                          OrdinalB_, std::cref(ErrorCallback));
  ASSERT_TRUE(CB.hasValue());
  ReturnType CA = Resolver_.defineSymbols(Undefs, &Globals, *CompilationB,
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
  rld::referenceSymbol(Ctx_, this->storeString(Name), S0, &Globals, Undefs);
  EXPECT_EQ(S0.size(), 0U);
  ASSERT_EQ(Globals.size(), 1U);
  EXPECT_FALSE(this->getSymbol(Globals, 0U).definition().hasValue());

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
      Undefs, &Globals, *C1, InputNo, std::cref(ErrorCallback));
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
                                   *S1, &Globals, Undefs),
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
// linkage, then the internal symbol is only visible inside of its defining CU.
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

  ReturnType C0 = Resolver_.defineSymbols(Undefs, &Globals,
                                          *this->compile(Name_, OtherLinkage),
                                          Input0_, std::cref(ErrorCallback));
  ASSERT_TRUE(C0.hasValue());
  ReturnType C1 = Resolver_.defineSymbols(
      Undefs, &Globals, *this->compile(Name_, linkage::internal), Input1_,
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
                                 &Globals, Undefs),
            &Symbol0);
  EXPECT_EQ(rld::referenceSymbol(Ctx_, getStringAddress(this->Db(), Name_), *C1,
                                 &Globals, Undefs),
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
      Undefs, &Globals, *this->compile(Name_, linkage::internal), Input0_,
      std::cref(ErrorCallback));
  ASSERT_TRUE(C0.hasValue());
  ReturnType C1 = Resolver_.defineSymbols(Undefs, &Globals,
                                          *this->compile(Name_, OtherLinkage),
                                          Input1_, std::cref(ErrorCallback));

  // Check that the resolver did the right thing. First that the symbol table is
  // as expected.
  ASSERT_TRUE(C1.hasValue());
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
                                 &Globals, Undefs),
            &Symbol0);
  EXPECT_EQ(rld::referenceSymbol(Ctx_, getStringAddress(this->Db(), Name_), *C1,
                                 &Globals, Undefs),
            &Symbol1);
}

INSTANTIATE_TEST_CASE_P(InternalWithCollision, InternalCollision,
                        testing::Values(
                            /*pstore::repo::linkage::append,*/
                            linkage::common, linkage::external,
                            linkage::internal, linkage::link_once_any,
                            linkage::link_once_odr, linkage::weak_any,
                            linkage::weak_odr), );
