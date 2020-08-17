//*                    *
//*   __ _  ___ _ __   *
//*  / _` |/ _ \ '_ \  *
//* | (_| |  __/ | | | *
//*  \__, |\___|_| |_| *
//*  |___/             *
//===- tools/gen/gen.cpp --------------------------------------------------===//
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

#include "FragmentCreator.h"
#include "fibonacci.hpp"
#include "hash.h"

using namespace std::string_literals;

namespace {

llvm::cl::opt<std::string> DbPath("repo",
                                  llvm::cl::desc("Program repository path"),
                                  llvm::cl::init("clang.db"));
llvm::cl::opt<unsigned>
    AppendPerModule("append",
                    llvm::cl::desc("Number of append symbols per module"),
                    llvm::cl::init(1U));
llvm::cl::opt<unsigned>
    CommonPerModule("common",
                    llvm::cl::desc("Number of common symbols per module"),
                    llvm::cl::init(1U));
llvm::cl::opt<unsigned>
    ExternalPerModule("external",
                      llvm::cl::desc("Number of external symbols per module"),
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
llvm::cl::alias
    OutputDirOptAlias("O", llvm::cl::desc("Alias for --output-directory"),
                      llvm::cl::aliasopt(OutputDirOpt));

llvm::cl::opt<unsigned> SectionSize(
    "section-size",
    llvm::cl::desc("Number of 32-bit values in the generated sections"),
    llvm::cl::init(16U));

llvm::cl::opt<std::string>
    Triple("triple",
           llvm::cl::desc("The target-triple associated with each compilation"),
           llvm::cl::init("x86_64-pc-linux-gnu-repo"));

llvm::cl::opt<bool> DataFibonacci(
    "data-fibonacci",
    llvm::cl::desc(
        "Fills the generated section data with a fibonacci sequence"),
    llvm::cl::init(false));

llvm::cl::opt<bool>
    Progress("progress",
             llvm::cl::desc("Enables progress monitoring on stdout"),
             llvm::cl::init(false));
llvm::cl::alias ProgressAlias("p", llvm::cl::desc("Alias for --output"),
                              llvm::cl::aliasopt(Progress));

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

class SharedFragments {
public:
  using Entry = std::tuple<
      std::pair<pstore::index::digest, pstore::extent<pstore::repo::fragment>>,
      IStringAddress>;

  SharedFragments(
      std::shared_ptr<pstore::index::fragment_index> const &FragmentIndex,
      unsigned Count, const llvm::StringRef &NamePrefix)
      : Count_{Count}, NamePrefix_{NamePrefix}, FragmentIndex_{FragmentIndex} {

    Fragments_.reserve(Count);
  }

  SharedFragments(SharedFragments const &) = delete;
  SharedFragments(SharedFragments &&) = delete;
  SharedFragments &operator=(SharedFragments const &) = delete;
  SharedFragments &operator=(SharedFragments &&) = delete;

  unsigned
  createFragments(pstore::transaction<pstore::transaction_lock> &Transaction,
                  FragmentCreator &Creator, StringAdder &Strings) {
    for (auto Ctr = 0U; Ctr < Count_; ++Ctr) {
      auto Name = NamePrefix_ + std::to_string(Ctr);
      Fragments_.emplace_back(Creator(Transaction, Ctr),
                              Strings.add(Transaction, std::move(Name)));
      FragmentIndex_->insert(Transaction, std::get<0>(Fragments_.back()));
    }
    return Count_;
  }

  std::vector<Entry> const &fragments() const {
    assert(Fragments_.size() == Count_);
    return Fragments_;
  }

  unsigned const Count_;
  std::string const NamePrefix_;
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

static std::string getRepositoryPath() {
  if (DbPath.getNumOccurrences() > 0) {
    return DbPath.getValue();
  }
  llvm::SmallString<128> Result;
  Result = OutputDirOpt;
  llvm::sys::path::append(Result, "repo.db");
  return std::string(Result);
}

static llvm::SmallString<128> quoteAndEscape (llvm::StringRef const & S) {
    llvm::SmallString<128> Result;
    Result += '"';
    for (auto C: S) {
        if (C == '\\' || C == '"') {
            Result += '\\';
        }
        Result += C;
    }
    Result += '"';
    return Result;
}

static std::error_code writeConfigFile(llvm::StringRef const & Dir) {
  auto const AsBool = [](bool b) { return b ? "true" : "false"; };

  llvm::SmallString<128> ConfigFilePath = Dir;
  llvm::sys::path::append(ConfigFilePath, "rld-gen.json");

  std::error_code EC;
  llvm::raw_fd_ostream OutFile(ConfigFilePath, EC);
  if (EC) {
    return EC;
  }

  constexpr auto Indent = "    ";
  OutFile << "{\n";
  OutFile << Indent << R"("append": )" << AppendPerModule << ",\n";
  OutFile << Indent << R"("common": )" << CommonPerModule << ",\n";
  OutFile << Indent << R"("data-fibonacci": )" << AsBool(DataFibonacci)
          << ",\n";
  OutFile << Indent << R"("external": )" << ExternalPerModule << ",\n";
  OutFile << Indent << R"("linkonce": )" << LinkOncePerModule << ",\n";
  OutFile << Indent << R"("modules": )" << Modules << ",\n";
  OutFile << Indent << R"("output-directory": )" << quoteAndEscape(OutputDirOpt)
          << ",\n";
  OutFile << Indent << R"("repo-path": )" << quoteAndEscape(getRepositoryPath())
          << ",\n";
  OutFile << Indent << R"("section-size": )" << SectionSize << ",\n";
  OutFile << Indent << R"("triple": )" << quoteAndEscape(Triple) << "\n}\n";
  return OutFile.error();
}


int main(int argc, char *argv[]) {
  llvm::cl::ParseCommandLineOptions(argc, argv,
                                    "Repository Test Data Generator");

  static auto const ExternalPrefix = "external_"s;
  static auto const AppendPrefix = "append_"s;

  pstore::database Db{getRepositoryPath(), pstore::database::access_mode::writable};

  Indices Idx{Db};
  SharedFragments LinkOnceInfo{Idx.Fragments, LinkOncePerModule, "linkonce_"s};
  SharedFragments CommonInfo{Idx.Fragments, CommonPerModule, "common_"s};

  auto Transaction = pstore::begin(Db);
  StringAdder Strings{(ExternalPerModule + 1) * Modules + LinkOncePerModule +
                          CommonPerModule + AppendPerModule + 1,
                      Idx.Names};

  llvm::SmallString<128> OutputDir = llvm::StringRef{OutputDirOpt};
  if (const std::error_code EC = llvm::sys::fs::make_absolute(OutputDir)) {
    llvm::errs() << "failed to obtain absolute path for " << OutputDir << '\n';
    return EXIT_FAILURE;
  }

  if (const std::error_code EC = writeConfigFile(OutputDir)) {
    llvm::errs() << "failed to write configuration file\n";
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
      AppendPerModule + CommonPerModule + ExternalPerModule + LinkOncePerModule;
  auto FragmentCount = 0U;
  auto ExternalCount = 0U;

  FragmentCreator BssCreator{
      DataFibonacci, SectionSize,
      SectionSet{
          (1ULL << static_cast<unsigned>(pstore::repo::section_kind::bss))}};

  for (auto ModuleCtr = 0U; ModuleCtr < Modules; ++ModuleCtr) {
    FragmentCreator FCreator{
        DataFibonacci, SectionSize,
        SectionSet{
            (1ULL << static_cast<unsigned>(pstore::repo::section_kind::text)) |
            (1ULL << static_cast<unsigned>(
                 pstore::repo::section_kind::read_only))}};

    if (ModuleCtr == 0) {
      FragmentCount +=
          LinkOnceInfo.createFragments(Transaction, FCreator, Strings);
      FragmentCount +=
          CommonInfo.createFragments(Transaction, BssCreator, Strings);
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

    assert(LinkOnceInfo.fragments().size() == LinkOncePerModule);
    for (auto const &LO : LinkOnceInfo.fragments()) {
      auto const &DigestExtentPair = std::get<0>(LO);
      CompilationMembers.emplace_back(DigestExtentPair.first,
                                      DigestExtentPair.second, std::get<1>(LO),
                                      pstore::repo::linkage::link_once_any);
    }
    assert(CommonInfo.fragments().size() == CommonPerModule);
    for (auto const &Common : CommonInfo.fragments()) {
      auto const &DigestExtentPair = std::get<0>(Common);
      CompilationMembers.emplace_back(
          DigestExtentPair.first, DigestExtentPair.second, std::get<1>(Common),
          pstore::repo::linkage::common);
    }
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
    if (Progress) {
      std::cout << ModuleCtr << std::endl;
    }
  }

  // Write the bodies of the strings.
  Strings.flush(Transaction);
  Transaction.commit();

  return EXIT_SUCCESS;
}
