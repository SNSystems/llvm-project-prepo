//===- tools/gen/gen.cpp --------------------------------------------------===//
//*                    *
//*   __ _  ___ _ __   *
//*  / _` |/ _ \ '_ \  *
//* | (_| |  __/ | | | *
//*  \__, |\___|_| |_| *
//*  |___/             *
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/Twine.h"
#include "llvm/MC/MCRepoTicketFile.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/EndianStream.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"

#include "pstore/core/hamt_map.hpp"
#include "pstore/core/hamt_set.hpp"
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
#include "StringAdder.h"
#include "fibonacci.h"
#include "hash.h"

using namespace std::string_literals;

namespace {

llvm::cl::opt<std::string> DbPath("repo",
                                  llvm::cl::desc("Program repository path"),
                                  llvm::cl::init("clang.db"));
llvm::cl::opt<unsigned>
    AppendPerModule("append",
                    llvm::cl::desc("Number of append symbols per module"),
                    llvm::cl::init(0U));
llvm::cl::opt<unsigned>
    CommonPerModule("common",
                    llvm::cl::desc("Number of common symbols per module"),
                    llvm::cl::init(0U));
llvm::cl::opt<unsigned>
    ExternalPerModule("external",
                      llvm::cl::desc("Number of external symbols per module"),
                      llvm::cl::init(0U));
llvm::cl::opt<unsigned>
    LinkOncePerModule("linkonce",
                      llvm::cl::desc("Number of link-once symbols per module"),
                      llvm::cl::init(0U));

llvm::cl::opt<unsigned> Modules("modules", llvm::cl::desc("Number of modules"),
                                llvm::cl::init(1U));
llvm::cl::opt<std::string> OutputDirOpt("output-directory",
                                        llvm::cl::desc("output directory"),
                                        llvm::cl::init("./"));
llvm::cl::alias
    OutputDirOptAlias("O", llvm::cl::desc("Alias for --output-directory"),
                      llvm::cl::aliasopt(OutputDirOpt));

llvm::cl::opt<unsigned> SectionSize{
    "section-size",
    llvm::cl::desc{"Number of 32-bit values in the generated sections"},
    llvm::cl::init(16U)};

llvm::cl::opt<unsigned> XFixupSize{
    "xfixup-size", llvm::cl::desc{"Number of external fixups in a section"},
    llvm::cl::init(0U)};

llvm::cl::opt<unsigned> IFixupSize{
    "ifixup-size", llvm::cl::desc{"Number of internal fixups in a section"},
    llvm::cl::init(0U)};

llvm::cl::opt<std::string::size_type> PrefixLength{
    "prefix-length",
    llvm::cl::desc{"Length of prefix applied to generated names"},
    llvm::cl::init(6)};

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



} // end anonymous namespace

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
      const IStringAddress SA =
          Strings.add(Transaction, NamePrefix_ + std::to_string(Ctr),
                      true /*IsDefinition*/);
      Fragments_.emplace_back(Creator(Transaction, Ctr, Strings), SA);
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
  explicit Indices(pstore::database &Db)
      : Fragments{pstore::index::get_index<pstore::trailer::indices::fragment>(
            Db)},
        Compilations{
            pstore::index::get_index<pstore::trailer::indices::compilation>(
                Db)},
        Names{pstore::index::get_index<pstore::trailer::indices::name>(Db)},
        Paths{pstore::index::get_index<pstore::trailer::indices::path>(Db)} {}

  std::shared_ptr<pstore::index::fragment_index> Fragments;
  std::shared_ptr<pstore::index::compilation_index> Compilations;
  std::shared_ptr<pstore::index::name_index> Names;
  std::shared_ptr<pstore::index::path_index> Paths;
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

static llvm::Error writeConfigFile(llvm::StringRef const &Dir) {
  auto const AsBool = [](bool b) { return b ? "true" : "false"; };

  llvm::SmallString<128> ConfigFilePath = Dir;
  llvm::sys::path::append(ConfigFilePath, "rld-gen.json");

  std::error_code EC;
  llvm::raw_fd_ostream OutFile(ConfigFilePath, EC);
  if (EC) {
    return llvm::errorCodeToError(EC);
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
  return llvm::errorCodeToError(OutFile.error());
}

static llvm::Error checkIsDirectory(llvm::StringRef OutputDir) {
  llvm::sys::fs::file_status FS;
  if (const std::error_code EC = llvm::sys::fs::status(OutputDir, FS)) {
    return llvm::errorCodeToError(EC);
  }
  if (!llvm::sys::fs::is_directory(FS)) {
    return llvm::errorCodeToError(make_error_code(std::errc::not_a_directory));
  }
  return llvm::ErrorSuccess{};
}

int main(int argc, char *argv[]) {
  llvm::cl::ParseCommandLineOptions(argc, argv,
                                    "Repository Test Data Generator");

  //  static constexpr std::string::size_type NameBloat = Pr64 * 1024;
  static const auto AppendPrefix = std::string(PrefixLength, 'a') + '_';
  static const auto CommonPrefix = std::string(PrefixLength, 'c') + '_';
  static const auto ExternalPrefix = std::string(PrefixLength, 'x') + '_';
  static const auto LinkOncePrefix = std::string(PrefixLength, 'l') + '_';

  int ExitCode = EXIT_SUCCESS;
  llvm::ExitOnError ExitOnErr("Error: ", EXIT_FAILURE);

  pstore::database Db{getRepositoryPath(), pstore::database::access_mode::writable};

  Indices Idx{Db};
  SharedFragments LinkOnceInfo{Idx.Fragments, LinkOncePerModule,
                               LinkOncePrefix};
  SharedFragments CommonInfo{Idx.Fragments, CommonPerModule, CommonPrefix};

  const llvm::StringRef OutputDir{OutputDirOpt};
  ExitOnErr(checkIsDirectory(OutputDir));
  const auto FullOutputDir =
      ExitOnErr(llvm::mc::repo::realTicketDirectory(OutputDir));
  ExitOnErr(writeConfigFile(FullOutputDir));

  auto Transaction = pstore::begin(Db);

  llvm::mc::repo::recordTicketDirectory(Transaction, Idx.Paths, FullOutputDir);

  StringAdder Strings{(ExternalPerModule + 1) * Modules + LinkOncePerModule +
                          CommonPerModule + AppendPerModule + 1,
                      Idx.Names};
  IStringAddress TripleName = Strings.add(Transaction, Triple, false);

  // Create names for the append symbols.
  std::vector<IStringAddress> AppendNames;
  AppendNames.reserve(AppendPerModule);
  for (auto Ctr = 0U; Ctr < AppendPerModule; ++Ctr) {
    AppendNames.emplace_back(
        Strings.add(Transaction, std::move(AppendPrefix + std::to_string(Ctr)),
                    true /*IsDefinition*/));
  }

  auto const SymbolsPerModule =
      AppendPerModule + CommonPerModule + ExternalPerModule + LinkOncePerModule;
  auto FragmentCount = 0U;
  auto ExternalCount = 0U;

  FragmentCreator BssCreator{
      DataFibonacci, SectionSize, 0U, 0U,
      SectionSet{
          (1ULL << static_cast<unsigned>(pstore::repo::section_kind::bss))}};

  for (auto ModuleCtr = 0U; ModuleCtr < Modules; ++ModuleCtr) {
    FragmentCreator FCreator{
        DataFibonacci, SectionSize, XFixupSize, IFixupSize,
        SectionSet{
            (1ULL << static_cast<unsigned>(pstore::repo::section_kind::text)) |
            (1ULL << static_cast<unsigned>(
                 pstore::repo::section_kind::read_only))}};

    // Create the fragments that are referenced by the link-once and BSS
    // definitions. These are shared by the individual compilations so are only
    // created on the first pass throug this loop.
    if (ModuleCtr == 0) {
      FragmentCount +=
          LinkOnceInfo.createFragments(Transaction, FCreator, Strings);
      FragmentCount +=
          CommonInfo.createFragments(Transaction, BssCreator, Strings);
    }

    std::vector<pstore::repo::definition> Definitions;
    Definitions.reserve(SymbolsPerModule);

    // Create the per-compilation external definitions along with their (unique)
    // fragments.

    for (auto ExternalCtr = 0U; ExternalCtr < ExternalPerModule;
         ++ExternalCtr) {
      const IStringAddress Name = Strings.add(
          Transaction, ExternalPrefix + std::to_string(ExternalCount++),
          true /*IsDefinition*/);
      auto const DigestExtentPair =
          FCreator(Transaction, FragmentCount++, Strings);
      Definitions.emplace_back(DigestExtentPair.first, DigestExtentPair.second,
                               Name, pstore::repo::linkage::external);
    }

    // Create the per-compilation 'append' defintions along with their
    // fragments.

    for (auto AppendCtr = 0U; AppendCtr < AppendPerModule; ++AppendCtr) {
      auto const DigestExtentPair =
          FCreator(Transaction, FragmentCount++, Strings);
      Definitions.emplace_back(DigestExtentPair.first, DigestExtentPair.second,
                               AppendNames[AppendCtr],
                               pstore::repo::linkage::append);
    }

    for (auto const &Def : Definitions) {
      Idx.Fragments->insert(Transaction, std::make_pair(Def.digest, Def.fext));
    }

    assert(LinkOnceInfo.fragments().size() == LinkOncePerModule);
    for (auto const &LO : LinkOnceInfo.fragments()) {
      auto const &DigestExtentPair = std::get<0>(LO);
      Definitions.emplace_back(DigestExtentPair.first, DigestExtentPair.second,
                               std::get<1>(LO),
                               pstore::repo::linkage::link_once_any);
    }
    assert(CommonInfo.fragments().size() == CommonPerModule);
    for (auto const &Common : CommonInfo.fragments()) {
      auto const &DigestExtentPair = std::get<0>(Common);
      Definitions.emplace_back(DigestExtentPair.first, DigestExtentPair.second,
                               std::get<1>(Common),
                               pstore::repo::linkage::common);
    }
    assert(Definitions.size() == SymbolsPerModule);

    {
      // Create an entry in the compilation index.
      auto Compilation = pstore::repo::compilation::alloc(
          Transaction, TripleName, std::begin(Definitions),
          std::end(Definitions));
      pstore::index::digest const ModuleDigest =
          rawHash(Transaction.db(), Compilation);
      Idx.Compilations->insert(Transaction,
                               std::make_pair(ModuleDigest, Compilation));

      // Write the ticket file to disk.
      ExitOnErr(llvm::mc::repo::writeTicketFile(
          getTicketFilePath(llvm::StringRef{OutputDirOpt}, ModuleCtr),
          Transaction.db(), ModuleDigest));
    }

    if (Progress) {
      llvm::outs() << ModuleCtr << '\n';
      llvm::outs().flush();
    }
  }

  // Write the bodies of the strings.
  Strings.flush(Transaction);
  Transaction.commit();

  return ExitCode;
}
