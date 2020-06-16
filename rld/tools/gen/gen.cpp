//*                    *
//*   __ _  ___ _ __   *
//*  / _` |/ _ \ '_ \  *
//* | (_| |  __/ | | | *
//*  \__, |\___|_| |_| *
//*  |___/             *
//===- tools/gen/gen.cpp --------------------------------------------------===//
// Copyright (c) 2017-2018 by Sony Interactive Entertainment, Inc.
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

#include "llvm/Support/CommandLine.h"

#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"

#include "pstore/core/hamt_map.hpp"
#include "pstore/core/hamt_set.hpp"
#include "pstore/core/index_types.hpp"
#include "pstore/core/sstring_view_archive.hpp"
#include "pstore/core/transaction.hpp"
#include "pstore/mcrepo/compilation.hpp"
#include "pstore/mcrepo/fragment.hpp"
#include "pstore/os/path.hpp"
#include "pstore/support/pointee_adaptor.hpp"
#include "pstore/support/portab.hpp"

#include <bitset>
#include <fstream>
#include <list>
#include <sstream>
#include <string>
#include <vector>

#include "fibonacci.hpp"
#include "hash.h"

using namespace std::string_literals;

namespace {

template <typename IntegerType>
class ConstGenerator
    : public std::iterator<std::forward_iterator_tag, IntegerType> {
public:
  using result_type = IntegerType;

  explicit constexpr ConstGenerator(IntegerType V) noexcept : Constant_{V} {}
  ConstGenerator(ConstGenerator const &Rhs) = default;
  ConstGenerator &operator=(ConstGenerator const &Rhs) = default;

  constexpr bool operator==(ConstGenerator const &Rhs) const {
    return Constant_ == Rhs.Constant_;
  }
  constexpr bool operator!=(ConstGenerator const &Rhs) const {
    return !operator==(Rhs);
  }

  constexpr IntegerType operator*() const { return Constant_; }
  constexpr ConstGenerator &operator++() { return *this; }
  constexpr ConstGenerator operator++(int) {
    ConstGenerator old = *this;
    ++(*this);
    return old;
  }

private:
  IntegerType Constant_;
};

} // namespace

namespace {

llvm::cl::opt<std::string> DbPath("repo",
                                  llvm::cl::desc("Program repository path"),
                                  llvm::cl::init("clang.db"));
llvm::cl::opt<unsigned>
    ExternalPerModule("external",
                      llvm::cl::desc("Number of external symbols per module"),
                      llvm::cl::init(1U));
llvm::cl::opt<unsigned>
    AppendPerModule("append",
                    llvm::cl::desc("Number of append symbols per module"),
                    llvm::cl::init(1U));
llvm::cl::opt<unsigned>
    LinkOncePerModule("linkonce",
                      llvm::cl::desc("Number of link-once symbols per module"),
                      llvm::cl::init(1U));

llvm::cl::opt<unsigned> Modules("modules", llvm::cl::desc("Number of modules"),
                                llvm::cl::init(1U));
llvm::cl::opt<std::string> OutputDirOpt("output-directory",
                                        llvm::cl::desc("output directory"),
                                        llvm::cl::init("./"));
llvm::cl::alias OutputDirOpt2("O",
                              llvm::cl::desc("Alias for --output-directory"),
                              llvm::cl::aliasopt(OutputDirOpt));

llvm::cl::opt<unsigned> SectionSize(
    "section-size",
    llvm::cl::desc("Number of 32-bit values in the generated sections"),
    llvm::cl::init(16U));

llvm::cl::opt<std::string>
    Triple("triple",
           llvm::cl::desc("The target-triple associated with each compilation"),
           llvm::cl::init("x86_64-pc-linux-gnu-repo"));

llvm::cl::opt<bool> DataFibonacci("data-fibonacci", llvm::cl::init(false));

using IStringAddress = pstore::typed_address<pstore::indirect_string>;

class StringAdder {
public:
  StringAdder(size_t ExpectedStrings,
              std::shared_ptr<pstore::index::name_index> const &NamesIndex)
      : NamesIndex_{NamesIndex}, Adder_{ExpectedStrings} {
    Strings_.reserve(ExpectedStrings);
  }

  template <typename Lock>
  IStringAddress add(pstore::transaction<Lock> &Transaction,
                     const llvm::StringRef &Str) {
    return addIndirect(Transaction, append(Str));
  }

  template <typename Lock> void flush(pstore::transaction<Lock> &transaction) {
    Adder_.flush(transaction);
  }

private:
  template <typename Lock>
  auto addIndirect(pstore::transaction<Lock> &Transaction,
                   pstore::raw_sstring_view &View) -> IStringAddress {
    auto const add_res = Adder_.add(Transaction, NamesIndex_, &View);
    if (!add_res.second) {
      // Already there! Delete the last entry
      assert(false);
    }
    return IStringAddress::make(add_res.first.get_address());
  }

  template <typename StringType>
  pstore::raw_sstring_view &append(StringType &&Str) {
    assert(Strings_.size() + 1 <= Strings_.capacity());
    Strings_.emplace_back(std::forward<StringType>(Str),
                          pstore::raw_sstring_view{});
    auto &back = Strings_.back();
    back.second =
        pstore::raw_sstring_view{back.first.data(), back.first.size()};
    return back.second;
  }

  std::shared_ptr<pstore::index::name_index> NamesIndex_;
  pstore::indirect_string_adder Adder_;

  std::vector<std::pair<std::string, pstore::raw_sstring_view>> Strings_;
};

} // end anonymous namespace

// FIXME: a variation is found in MCRepoTicketFile.cpp
static std::error_code writeTicketFile(llvm::StringRef const &Path,
                                pstore::index::digest const &digest) {
  static constexpr auto magic_size = size_t{8};
#if PSTORE_IS_BIG_ENDIAN
  static constexpr std::array<char, MagicSize> be_magic{
      {'R', 'e', 'p', 'o', 'T', 'c', 'k', 't'}};
  constexpr auto signature = &be_magic;
#else
  static constexpr std::array<char, magic_size> le_magic{
      {'t', 'k', 'c', 'T', 'o', 'p', 'e', 'R'}};
  constexpr auto signature = &le_magic;
#endif

  std::error_code EC;
  llvm::raw_fd_ostream OutFile(Path, EC);
  if (EC) {
    return EC;
  }
  OutFile.write(signature->data(), signature->size());
  OutFile.write(reinterpret_cast<char const *>(&digest), sizeof(digest));
  return OutFile.error();
}

static void addSectionToHash(HashFunction *const Hash,
                             pstore::repo::section_content const &Section) {
  hashNumber(*Hash, Section.kind);
  hashNumber(*Hash, Section.align);
  hashNumber(*Hash, Section.data.size());
  Hash->update(llvm::makeArrayRef(Section.data.data(), Section.data.size()));
  hashNumber(*Hash, Section.ifixups.size());
  // FIXME: hash the fixups (once we generate them).
  // Hash.update (section.ifixups);
  hashNumber(*Hash, Section.xfixups.size());
  // Hash.update (section.xfixups);
}

namespace {

template <typename T>
pstore::index::digest rawHash(pstore::database const &db,
                              pstore::extent<T> const &ext) {
  HashFunction Hash;

  std::shared_ptr<T const> const p = db.getro(ext);
  Hash.update(llvm::ArrayRef<uint8_t>{
      reinterpret_cast<std::uint8_t const *>(p.get()), ext.size});
  return Hash.finalize();
}

constexpr auto MaxSection =
    static_cast<size_t>(pstore::repo::section_kind::last);
using SectionSet = std::bitset<MaxSection>;

class FragmentCreator {
public:
  using Transaction = pstore::transaction<pstore::transaction_lock>;
  using FragmentIndex = std::shared_ptr<pstore::index::fragment_index>;
  using FragmentIndexValueType = pstore::index::fragment_index::value_type;

  explicit FragmentCreator(SectionSet const &Sections);
  auto operator()(Transaction &T, size_t const Count) -> FragmentIndexValueType;

private:
  template <typename Generator>
  auto create(Transaction &T, Generator &&G, size_t const Count)
      -> FragmentIndexValueType;

  template <typename Generator>
  pstore::repo::section_content
  generateDataSection(Generator &G, pstore::repo::section_kind Kind);

  using DispatcherPtr =
      std::unique_ptr<pstore::repo::section_creation_dispatcher>;

  template <pstore::repo::section_kind Kind> DispatcherPtr createDispatcher();

  template <pstore::repo::section_kind Kind,
            typename DispatcherType =
                typename pstore::repo::section_to_creation_dispatcher<
                    typename pstore::repo::enum_to_section<Kind>::type>::type>
  void
  setDispatcherContent(pstore::repo::section_creation_dispatcher *dispatcher,
                       pstore::repo::section_content const *Content);

  SectionSet const Sections_;
  fibonacci_generator<> Fib_;
  llvm::SmallVector<DispatcherPtr, MaxSection> Dispatchers_;
};

// createDispatcher
// ~~~~~~~~~~~~~~~~
template <pstore::repo::section_kind Kind>
auto FragmentCreator::createDispatcher() -> DispatcherPtr {
  using SectionType = typename pstore::repo::enum_to_section<Kind>::type;
  using DispatcherType =
      typename pstore::repo::section_to_creation_dispatcher<SectionType>::type;
  return DispatcherPtr{new DispatcherType(Kind)};
}
template <>
auto FragmentCreator::createDispatcher<pstore::repo::section_kind::bss>()
    -> DispatcherPtr {
  return DispatcherPtr{new pstore::repo::bss_section_creation_dispatcher()};
}
template <>
auto FragmentCreator::createDispatcher<pstore::repo::section_kind::debug_line>()
    -> DispatcherPtr {
  return {};
}
template <>
auto FragmentCreator::createDispatcher<pstore::repo::section_kind::dependent>()
    -> DispatcherPtr {
  return {};
}

// setDispatcherContent
// ~~~~~~~~~~~~~~~~~~~~
template <pstore::repo::section_kind Kind, typename DispatcherType>
void FragmentCreator::setDispatcherContent(
    pstore::repo::section_creation_dispatcher *dispatcher,
    pstore::repo::section_content const *Content) {
  reinterpret_cast<DispatcherType *>(dispatcher)->set_content(Content);
}

template <>
void FragmentCreator::setDispatcherContent<
    pstore::repo::section_kind::debug_line>(
    pstore::repo::section_creation_dispatcher *dispatcher,
    pstore::repo::section_content const *Content) {}
template <>
void FragmentCreator::setDispatcherContent<
    pstore::repo::section_kind::dependent>(
    pstore::repo::section_creation_dispatcher *dispatcher,
    pstore::repo::section_content const *Content) {}

FragmentCreator::FragmentCreator(const SectionSet &Sections)
    : Sections_{Sections} {

  for (auto Bit = size_t{0}, MaxBit = Sections_.size(); Bit < MaxBit; ++Bit) {
    if (Sections_.test(Bit)) {
      const auto Kind = static_cast<pstore::repo::section_kind>(Bit);
#define X(x)                                                                   \
  case pstore::repo::section_kind::x:                                          \
    Dispatchers_.emplace_back(                                                 \
        createDispatcher<pstore::repo::section_kind::x>());                    \
    break;

      switch (Kind) {
        PSTORE_MCREPO_SECTION_KINDS
      case pstore::repo::section_kind::last:
        break;
      }
#undef X
    }
  }
}

// operator ()
// ~~~~~~~~~~~
auto FragmentCreator::operator()(Transaction &T, size_t const Count)
    -> FragmentIndexValueType {
  if (DataFibonacci) {
    return create(T, Fib_, Count);
  }
  return create(T, ConstGenerator<size_t>{Count}, Count);
}

// create
// ~~~~~~
template <typename Generator>
auto FragmentCreator::create(Transaction &T, Generator &&G, size_t const Count)
    -> FragmentIndexValueType {

  HashFunction FragmentHash;
  hashNumber(FragmentHash, Count);
  hashNumber(FragmentHash, Sections_.size());

  llvm::SmallVector<pstore::repo::section_content, MaxSection> Contents;
  auto Index = size_t{0};
  for (auto Bit = size_t{0}, MaxBit = Sections_.size(); Bit < MaxBit; ++Bit) {
    if (Sections_.test(Bit)) {
      assert(Dispatchers_[Index]);

      auto const Kind = static_cast<pstore::repo::section_kind>(Bit);
      Contents.emplace_back(generateDataSection(G, Kind));
      auto const &Content = Contents.back();

      assert(Content.data.size() == SectionSize.getValue() * 4);
#define X(x)                                                                   \
  case pstore::repo::section_kind::x:                                          \
    setDispatcherContent<pstore::repo::section_kind::x>(                       \
        Dispatchers_[Index].get(), &Content);                                  \
    break;

      switch (Kind) {
        PSTORE_MCREPO_SECTION_KINDS
      case pstore::repo::section_kind::last:
        break;
      }

      hashNumber(FragmentHash, Bit);
      addSectionToHash(&FragmentHash, Content);
#undef X
      ++Index;
    }
  }

  pstore::index::digest const digest = FragmentHash.finalize();
  auto const First = pstore::make_pointee_adaptor(Dispatchers_.begin());
  auto const Last = pstore::make_pointee_adaptor(Dispatchers_.end());
  return std::make_pair(digest, pstore::repo::fragment::alloc(T, First, Last));
}

// generateDataSection
// ~~~~~~~~~~~~~~~~~~~
template <typename Generator>
pstore::repo::section_content
FragmentCreator::generateDataSection(Generator &G,
                                     pstore::repo::section_kind Kind) {
  pstore::repo::section_content DataSection{Kind,
                                            std::uint8_t{8} /*alignment*/};
  size_t const Size = SectionSize.getValue();
  DataSection.data.reserve(Size * 4U);
  for (auto Ctr = size_t{0}; Ctr < Size; ++Ctr) {
    auto V = *G;
    ++G;

    DataSection.data.emplace_back((V >> 24) & 0xFF);
    DataSection.data.emplace_back((V >> 16) & 0xFF);
    DataSection.data.emplace_back((V >> 8) & 0xFF);
    DataSection.data.emplace_back((V >> 0) & 0xFF);
  }
  return DataSection;
}

class LinkOnce {
public:
  using Entry = std::tuple<
      std::pair<pstore::index::digest, pstore::extent<pstore::repo::fragment>>,
      IStringAddress>;

  explicit LinkOnce(
      std::shared_ptr<pstore::index::fragment_index> const &FragmentIndex,
      unsigned LinkOncePerModule)
      : FragmentIndex_{FragmentIndex} {
    Fragments_.reserve(LinkOncePerModule);
  }

  LinkOnce(LinkOnce const &) = delete;
  LinkOnce(LinkOnce &&) = delete;
  LinkOnce &operator=(LinkOnce const &) = delete;
  LinkOnce &operator=(LinkOnce &&) = delete;

  void
  createFragments(pstore::transaction<pstore::transaction_lock> &Transaction,
                  FragmentCreator &Creator, StringAdder &Strings) {
    static auto const NamePrefix = "linkonce_"s;
    for (auto Ctr = 0U; Ctr < LinkOncePerModule; ++Ctr) {
      auto Name = NamePrefix + std::to_string(Ctr);
      Fragments_.emplace_back(Creator(Transaction, Ctr),
                              Strings.add(Transaction, std::move(Name)));
      FragmentIndex_->insert(Transaction, std::get<0>(Fragments_.back()));
    }
  }

  std::vector<Entry> const &fragments() const {
    assert(Fragments_.size() == LinkOncePerModule);
    return Fragments_;
  }

  // A mutex which protects access to the pstore APIs.
  std::shared_ptr<pstore::index::fragment_index> const &FragmentIndex_;
  std::vector<Entry> Fragments_;
};

struct Indices {
  Indices(pstore::database &Db)
      : Fragments{pstore::index::get_index<pstore::trailer::indices::fragment>(
            Db)},
        Compilations{
            pstore::index::get_index<pstore::trailer::indices::compilation>(
                Db)},
        Names{pstore::index::get_index<pstore::trailer::indices::name>(Db)} {}
  std::shared_ptr<pstore::index::fragment_index> Fragments;
  std::shared_ptr<pstore::index::compilation_index> Compilations;
  std::shared_ptr<pstore::index::name_index> Names;
};

} // end anonymous namespace

static llvm::SmallString<128> getTicketFilePath(llvm::StringRef OutputDir,
                                                unsigned ModuleCtr) {
  llvm::SmallString<128> TicketFilePath = OutputDir;

  llvm::SmallString<32> TicketFileName;
  llvm::raw_svector_ostream OS(TicketFileName);
  OS << 't' << ModuleCtr << ".o";

  llvm::sys::path::append(TicketFilePath, TicketFileName);
  return TicketFilePath;
}

int main(int argc, char *argv[]) {
  llvm::cl::ParseCommandLineOptions(argc, argv,
                                    "Repository Test Data Generator");

  static auto const ExternalPrefix = "external_"s;
  static auto const AppendPrefix = "append_"s;

  pstore::database Db{DbPath, pstore::database::access_mode::writable};

  Indices Idx{Db};
  LinkOnce LOInfo{Idx.Fragments, LinkOncePerModule};

  auto Transaction = pstore::begin(Db);
  StringAdder Strings{(ExternalPerModule + 1) * Modules + LinkOncePerModule +
                          AppendPerModule + 1,
                      Idx.Names};

  llvm::SmallString<128> OutputDir = llvm::StringRef{OutputDirOpt};
  if (std::error_code EC = llvm::sys::fs::make_absolute(OutputDir)) {
    llvm::errs() << "failed to obtain absolute path for " << OutputDir << '\n';
    return EXIT_FAILURE;
  }
  IStringAddress TicketPath = Strings.add(Transaction, OutputDir);
  IStringAddress TripleName = Strings.add(Transaction, Triple);

  // Create names for the append symbols.
  std::vector<IStringAddress> AppendNames;
  AppendNames.reserve(AppendPerModule);
  for (auto Ctr = 0U; Ctr < AppendPerModule; ++Ctr) {
    AppendNames.emplace_back(Strings.add(
        Transaction, std::move(AppendPrefix + std::to_string(Ctr))));
  }

  auto const SymbolsPerModule =
      ExternalPerModule + LinkOncePerModule + AppendPerModule;
  auto FragmentCount = 0U;
  auto ExternalCount = 0U;

  for (auto ModuleCtr = 0U; ModuleCtr < Modules; ++ModuleCtr) {
    FragmentCreator FCreator{SectionSet{
        (1ULL << static_cast<unsigned>(pstore::repo::section_kind::text)) |
        (1ULL << static_cast<unsigned>(
             pstore::repo::section_kind::read_only))}};

    if (ModuleCtr == 0) {
      LOInfo.createFragments(Transaction, FCreator, Strings);
      FragmentCount += LinkOncePerModule;
    }

    std::vector<pstore::repo::compilation_member> CompilationMembers;
    CompilationMembers.reserve(SymbolsPerModule);

    for (auto ExternalCtr = 0U; ExternalCtr < ExternalPerModule;
         ++ExternalCtr) {
      auto const DigestExtentPair = FCreator(Transaction, FragmentCount++);
      CompilationMembers.emplace_back(
          DigestExtentPair.first, DigestExtentPair.second,
          Strings.add(Transaction,
                      ExternalPrefix + std::to_string(ExternalCount++)),
          pstore::repo::linkage::external);
    }
    for (auto AppendCtr = 0U; AppendCtr < AppendPerModule; ++AppendCtr) {
      auto const DigestExtentPair = FCreator(Transaction, FragmentCount++);
      CompilationMembers.emplace_back(
          DigestExtentPair.first, DigestExtentPair.second,
          AppendNames[AppendCtr], pstore::repo::linkage::append);
    }

    for (auto const &CM : CompilationMembers) {
      Idx.Fragments->insert(Transaction, std::make_pair(CM.digest, CM.fext));
    }

    for (auto const &LO : LOInfo.fragments()) {
      auto const &DigestExtentPair = std::get<0>(LO);
      CompilationMembers.emplace_back(DigestExtentPair.first,
                                      DigestExtentPair.second, std::get<1>(LO),
                                      pstore::repo::linkage::link_once_any);
    }
    assert(LOInfo.fragments().size() == LinkOncePerModule);
    assert(CompilationMembers.size() == SymbolsPerModule);

    {
      auto Compilation = pstore::repo::compilation::alloc(
          Transaction, TicketPath, TripleName, std::begin(CompilationMembers),
          std::end(CompilationMembers));
      pstore::index::digest const ModuleDigest =
          rawHash(Transaction.db(), Compilation);
      Idx.Compilations->insert(Transaction,
                               std::make_pair(ModuleDigest, Compilation));
      writeTicketFile(getTicketFilePath(llvm::StringRef{OutputDir}, ModuleCtr),
                      ModuleDigest);
    }
    std::cout << ModuleCtr << std::endl;
  }

  // Write the bodies of the strings.
  Strings.flush(Transaction);
  Transaction.commit();

  return EXIT_SUCCESS;
}
