//===- repo2obj - Convert a Repository ticket to an ELF object file ------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/MC/MCRepoTicketFile.h"
#include "llvm/MC/StringTableBuilder.h"
#include "llvm/Object/Binary.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Support/raw_ostream.h"

#include "pstore/core/database.hpp"
#include "pstore/core/hamt_map.hpp"
#include "pstore/core/hamt_set.hpp"
#include "pstore/core/index_types.hpp"
#include "pstore/core/sstring_view_archive.hpp"
#include "pstore/mcrepo/compilation.hpp"
#include "pstore/mcrepo/fragment.hpp"
#include "pstore/support/array_elements.hpp"

#include "R2OELFOutputSection.h"
#include "WriteHelpers.h"

#include <cstdlib>
#include <functional>
#include <list>
#include <tuple>
#include <unordered_map>

#ifdef _WIN32
#include <io.h>
#endif

using namespace llvm;
using namespace llvm::object;

namespace {

cl::opt<std::string> RepoPath("repo", cl::Optional,
                              cl::desc("Program repository path"),
                              cl::init("./clang.db"));
cl::opt<std::string> TicketPath(cl::Positional, cl::desc("<ticket path>"));
cl::opt<std::string> OutputFilename("o", cl::desc("Output filename"),
                                    cl::value_desc("filename"),
                                    cl::init("./a.out"));

cl::opt<bool> NoSubsections(
    "no-subsections",
    cl::desc("Disable use of subsections for each code or data object"),
    cl::init(false));

constexpr auto program_name = "[repo2obj] ";

} // end anonymous namespace

LLVM_ATTRIBUTE_NORETURN static void error(Twine Message) {
  errs() << program_name << Message << "\n";
  exit(EXIT_FAILURE);
}

namespace {

class SpecialNames {
public:
  void initialize(const pstore::database &Db, GeneratedNames &Names);

  pstore::typed_address<pstore::indirect_string> CtorName =
      pstore::typed_address<pstore::indirect_string>::null();
  pstore::typed_address<pstore::indirect_string> DtorName =
      pstore::typed_address<pstore::indirect_string>::null();

private:
  static pstore::typed_address<pstore::indirect_string>
  findString(pstore::database const &Db,
             pstore::index::name_index const &NameIndex,
             pstore::indirect_string const &Str);
};

// initialize
// ~~~~~~~~~~
void SpecialNames::initialize(const pstore::database &Db, GeneratedNames &Names) {
  std::shared_ptr<pstore::index::name_index const> const NameIndex =
      pstore::index::get_index<pstore::trailer::indices::name>(Db);
  if (!NameIndex) {
    errs() << program_name << "Warning: name index was not found.\n";
  } else {
    // Get the address of the global tors names from the names set. If the
    // string is missing, use null since we know that can't appear as a ticket's
    // name.
    CtorName = findString(Db, *NameIndex, Names.add("llvm.global_ctors"));
    DtorName = findString(Db, *NameIndex, Names.add("llvm.global_dtors"));
  }
}

// findString
// ~~~~~~~~~~
pstore::typed_address<pstore::indirect_string>
SpecialNames::findString(pstore::database const &Db,
                         pstore::index::name_index const &NameIndex,
                         pstore::indirect_string const &Str) {
  auto const Pos = NameIndex.find(Db, Str);
  return (Pos != NameIndex.end(Db))
             ? pstore::typed_address<pstore::indirect_string>(Pos.get_address())
             : pstore::typed_address<pstore::indirect_string>::null();
}

template <class ELFT> struct ELFState {
  using Elf_Word = typename object::ELFFile<ELFT>::Elf_Word;
  using Elf_Ehdr = typename object::ELFFile<ELFT>::Elf_Ehdr;
  using Elf_Phdr = typename object::ELFFile<ELFT>::Elf_Phdr;
  using Elf_Shdr = typename object::ELFFile<ELFT>::Elf_Shdr;
  using Elf_Sym = typename object::ELFFile<ELFT>::Elf_Sym;
  using Elf_Rel = typename object::ELFFile<ELFT>::Elf_Rel;
  using Elf_Rela = typename object::ELFFile<ELFT>::Elf_Rela;

  using SectionHeaderTable = std::vector<Elf_Shdr>;
  SectionHeaderTable SectionHeaders;
  std::map<SectionId, OutputSection<ELFT>> Sections;
  std::map<pstore::typed_address<pstore::indirect_string>, GroupInfo<ELFT>>
      Groups;

  GeneratedNames Generated;
  StringTable Strings;
  SymbolTable<ELFT> Symbols;
  SpecialNames Magics;

  explicit ELFState(const pstore::database &Db)
      : Generated{Db}, Symbols{Strings} {}
  void initialize(const pstore::database &Db) { Magics.initialize(Db, Generated); }

  void initELFHeader(Elf_Ehdr &Header);
  void initStandardSections();
  uint64_t writeSectionHeaders(raw_ostream &OS);

  void buildGroupSection(const pstore::database &Db, GroupInfo<ELFT> &GI);

  /// Writes the group section data that was recorded by earlier calls to
  /// buildGroupSection(). The group section headers are updated to record the
  /// location and and size of this data.
  void writeGroupSections(raw_ostream &OS);

  /// Creates the section for extended symbol table section indices.
  size_t addSymtabShndxSection();

  /// Correctly sets the e_shnum field of the ELF header, potentially modifying
  /// the zeroth section header.
  /// \p Header The structure representing the ELF header.
  void setEShNum(Elf_Ehdr &Header);
};

// initELFHeader
// ~~~~~~~~~~~~~
template <class ELFT> void ELFState<ELFT>::initELFHeader(Elf_Ehdr &Header) {
  using namespace llvm::ELF;
  Header.e_ident[EI_MAG0] = 0x7f;
  Header.e_ident[EI_MAG1] = 'E';
  Header.e_ident[EI_MAG2] = 'L';
  Header.e_ident[EI_MAG3] = 'F';
  Header.e_ident[EI_CLASS] = ELFT::Is64Bits ? ELFCLASS64 : ELFCLASS32;
  Header.e_ident[EI_DATA] =
      (ELFT::TargetEndianness == support::little) ? ELFDATA2LSB : ELFDATA2MSB;
  Header.e_ident[EI_VERSION] = EV_CURRENT;
  Header.e_ident[EI_OSABI] = ELFOSABI_NONE;
  Header.e_ident[EI_ABIVERSION] = 0;
  Header.e_type = ET_REL;
  Header.e_machine =
      EM_X86_64; // FIXME: derive this from the compilation's triple.
  Header.e_version = EV_CURRENT;
  Header.e_entry = 0;
  Header.e_phoff = 0;
  Header.e_shoff = 0; // patched up later
  Header.e_flags = 0;
  Header.e_ehsize = sizeof(Elf_Ehdr);
  Header.e_phentsize = sizeof(Elf_Phdr);
  Header.e_phnum = 0;
  Header.e_shentsize = sizeof(Elf_Shdr);
  Header.e_shnum = 0; // patched up later.
  Header.e_shstrndx = SectionIndices::StringTab;
}

// initStandardSections
// ~~~~~~~~~~~~~~~~~~~~
template <typename ELFT> void ELFState<ELFT>::initStandardSections() {
  // null section
  Elf_Shdr SH;
  zero(SH);
  assert(SectionHeaders.size() == SectionIndices::Null);
  SectionHeaders.push_back(SH);

  // string table
  zero(SH);
  SH.sh_name = Strings.insert(Generated.add(".strtab"));
  SH.sh_type = ELF::SHT_STRTAB;
  assert(SectionHeaders.size() == SectionIndices::StringTab);
  SectionHeaders.push_back(SH);

  // Symbol table
  zero(SH);
  SH.sh_name = Strings.insert(Generated.add(".symtab"));
  SH.sh_type = ELF::SHT_SYMTAB;
  SH.sh_link = SectionIndices::StringTab;
  SH.sh_entsize = sizeof(ELFState<ELFT>::Elf_Sym);
  SH.sh_addralign = alignof(ELFState<ELFT>::Elf_Sym);
  assert(SectionHeaders.size() == SectionIndices::SymTab);
  SectionHeaders.push_back(SH);
}

// writeSectionHeaders
// ~~~~~~~~~~~~~~~~~~~
template <typename ELFT>
uint64_t ELFState<ELFT>::writeSectionHeaders(raw_ostream &OS) {
  writeAlignmentPadding<Elf_Shdr>(OS);
  auto const Offset = OS.tell();
  OS.write(reinterpret_cast<char const *>(SectionHeaders.data()),
           SectionHeaders.size() * sizeof(Elf_Shdr));
  return Offset;
}

// addSymtabShndxSection
// ~~~~~~~~~~~~~~~~~~~~~
template <typename ELFT> size_t ELFState<ELFT>::addSymtabShndxSection() {
  Elf_Shdr SH;
  zero(SH);
  SH.sh_name = Strings.insert(Generated.add(".symtab_shndx"));
  SH.sh_type = ELF::SHT_SYMTAB_SHNDX;
  SH.sh_link = SectionIndices::SymTab;
  SH.sh_entsize = sizeof(ELFState<ELFT>::Elf_Word);
  SH.sh_addralign = alignof(ELFState<ELFT>::Elf_Word);
  const size_t Index = SectionHeaders.size();
  SectionHeaders.push_back(SH);
  return Index;
}

// setEhNum
// ~~~~~~~~
template <typename ELFT> void ELFState<ELFT>::setEShNum(Elf_Ehdr &Header) {
  auto const NumberOfSections = SectionHeaders.size();
  // If the number of sections is greater than or equal to SHN_LORESERVE
  // (0xff00), e_shnum has the value zero. The actual number of section header
  // table entries is contained in the sh_size field of the section header at
  // index 0. Otherwise, the sh_size member of the initial section header entry
  // contains the value zero.
  if (NumberOfSections < llvm::ELF::SHN_LORESERVE) {
    Header.e_shnum = NumberOfSections;
    assert(SectionHeaders[0].sh_size == 0);
  } else {
    Header.e_shnum = 0;
    SectionHeaders[0].sh_size = NumberOfSections;
  }
}

// buildGroupSection
// ~~~~~~~~~~~~~~~~~
// The ELF spec. requires the groups sections to appear before the sections that
// they contain in the section header table. For this reason, we have to create
// them in two passes: the first creates the headers; later, once we know the
// collections of the section indices that they'll contain, we can write the
// group section bodies.
//
// A further wrinkle is that the entry in the section header table contains the
// index of the group's "signature" symbol. We therefore must have already
// generated and sorted the symbol table to assign indices.
template <typename ELFT>
void ELFState<ELFT>::buildGroupSection(pstore::database const &Db,
                                       GroupInfo<ELFT> &GI) {
  // If we haven't yet recorded a section index for this group, then build one
  // now.
  if (GI.SectionIndex == 0U) {
    auto SignatureSymbol = Symbols.findSymbol(
        pstore::indirect_string::read(Db, GI.IdentifyingSymbol));
    assert(SignatureSymbol != nullptr &&
           SignatureSymbol->Index != llvm::ELF::STN_UNDEF);
    static auto const GroupString = Generated.add(".group");
    ELFState<ELFT>::Elf_Shdr SH;
    zero(SH);
    SH.sh_name = Strings.insert(GroupString);
    SH.sh_type = ELF::SHT_GROUP;
    SH.sh_link = SectionIndices::SymTab;
    SH.sh_info = SignatureSymbol->Index; // The group's signature symbol entry.
    SH.sh_entsize = sizeof(ELF::Elf32_Word);
    SH.sh_addralign = alignof(ELF::Elf32_Word);

    GI.SectionIndex = SectionHeaders.size();
    assert(GI.SectionIndex != 0U);
    SectionHeaders.push_back(SH);
  }
}

// writeGroupSections
// ~~~~~~~~~~~~~~~~~~
template <typename ELFT>
void ELFState<ELFT>::writeGroupSections(raw_ostream &OS) {
  for (auto const &G : Groups) {
    writeAlignmentPadding<ELF::Elf32_Word>(OS);
    auto const StartPos = OS.tell();
    auto NumWords = 1U;
    writeRaw(OS, ELF::Elf32_Word{ELF::GRP_COMDAT});

    for (const OutputSection<ELFT> *const GroupMember : G.second.Members) {
      assert(GroupMember->getIndex() <= std::numeric_limits<uint32_t>::max());
      auto SectionIndex = static_cast<uint32_t>(GroupMember->getIndex());
      assert(SectionHeaders.at(SectionIndex).sh_type != ELF::SHT_GROUP &&
             SectionHeaders.at(SectionIndex).sh_type != ELF::SHT_RELA);
      writeRaw(OS, ELF::Elf32_Word{SectionIndex});
      ++NumWords;

      if (GroupMember->numRelocations() > 0) {
        // If the target section contains relocations then add that section to
        // the group as well. We know that it will always follow it immediately
        // in the section header table.
        ++SectionIndex;
        assert(SectionHeaders.at(SectionIndex).sh_type == ELF::SHT_RELA);
        writeRaw(OS, ELF::Elf32_Word{SectionIndex});
        ++NumWords;
      }
    }

    auto const SectionSize = NumWords * sizeof(ELF::Elf32_Word);
    assert(OS.tell() - StartPos == SectionSize);

    assert(G.second.SectionIndex < SectionHeaders.size());
    Elf_Shdr &SH = SectionHeaders[G.second.SectionIndex];
    assert(SH.sh_type == ELF::SHT_GROUP);
    SH.sh_offset = StartPos;
    SH.sh_size = SectionSize;
  }
}

} // end anonymous namespace

static ELFSectionType
getELFSectionType(pstore::repo::section_kind Kind,
                  pstore::typed_address<pstore::indirect_string> Name,
                  SpecialNames const &Magics) {
  if (Name == Magics.CtorName) {
    return ELFSectionType::init_array;
  } else if (Name == Magics.DtorName) {
    return ELFSectionType::fini_array;
  }

#define REPO_TO_ELF_SECTION(a)  case (pstore::repo::section_kind::a): return (ELFSectionType::a);

  switch (Kind) {
    REPO_TO_ELF_SECTION(bss)
    REPO_TO_ELF_SECTION(data)
    REPO_TO_ELF_SECTION(debug_line)
    REPO_TO_ELF_SECTION(debug_ranges)
    REPO_TO_ELF_SECTION(debug_string)
    REPO_TO_ELF_SECTION(interp)
    REPO_TO_ELF_SECTION(mergeable_1_byte_c_string)
    REPO_TO_ELF_SECTION(mergeable_2_byte_c_string)
    REPO_TO_ELF_SECTION(mergeable_4_byte_c_string)
    REPO_TO_ELF_SECTION(mergeable_const_16)
    REPO_TO_ELF_SECTION(mergeable_const_32)
    REPO_TO_ELF_SECTION(mergeable_const_4)
    REPO_TO_ELF_SECTION(mergeable_const_8)
    REPO_TO_ELF_SECTION(read_only)
    REPO_TO_ELF_SECTION(rel_ro)
    REPO_TO_ELF_SECTION(text)
    REPO_TO_ELF_SECTION(thread_bss)
    REPO_TO_ELF_SECTION(thread_data)
  case pstore::repo::section_kind::dependent:
  case pstore::repo::section_kind::last:
    break;
  }
#undef REPO_TO_ELF_SECTION
  llvm_unreachable("getELFSectionType: unknown repository section kind.");
}

static std::string getRepoPath() {
  if (RepoPath.getNumOccurrences() == 0) {
    // TODO: remove this envrionment variable once the matching behavior is
    // removed from the compiler.
    if (auto File = getenv("REPOFILE")) {
      return {File};
    }
  }
  return RepoPath;
}

raw_ostream &operator<<(raw_ostream &OS, pstore::index::digest const &Digest) {
  return OS << Digest.to_hex_string();
}

static constexpr bool
shouldCreateSubsection(bool IsLinkOnce,
                       pstore::repo::section_kind const Section) {
  switch (Section) {
  case pstore::repo::section_kind::debug_line:
  case pstore::repo::section_kind::debug_string:
  case pstore::repo::section_kind::debug_ranges:
    // Don't split the debug sections into subsections.
    return false;

  default:
    // Link-once always uses subsections; all other section types use them
    // unless disabled by the --no-subsections switch.
    return IsLinkOnce ? true : !NoSubsections;
  }
}

int main(int argc, char *argv[]) {
  cl::ParseCommandLineOptions(argc, argv);

  std::error_code EC;
  std::unique_ptr<ToolOutputFile> Out(
      new ToolOutputFile(OutputFilename, EC, sys::fs::F_None));
  if (EC) {
    error("Error opening '" + OutputFilename + "': " + EC.message());
  }

  ErrorOr<pstore::index::digest> DigestOrError =
      llvm::repo::getTicketIdFromFile(TicketPath);
  if (!DigestOrError) {
    error("Error: '" + TicketPath + "' (" + DigestOrError.getError().message() +
          ")");
    return EXIT_FAILURE;
  }

  pstore::index::digest const &Digest = DigestOrError.get();
  LLVM_DEBUG(dbgs() << "'" << TicketPath << "' : " << Digest << '\n');

  pstore::database Db(getRepoPath(), pstore::database::access_mode::read_only);
  std::shared_ptr<pstore::index::compilation_index const> const CompilationIndex =
      pstore::index::get_index<pstore::trailer::indices::compilation>(Db);
  if (!CompilationIndex) {
    error("Error: compilation index was not found.");
    return EXIT_FAILURE;
  }
  std::shared_ptr<pstore::index::fragment_index const> const FragmentIndex =
      pstore::index::get_index<pstore::trailer::indices::fragment>(Db);
  if (!FragmentIndex) {
    error("Error: fragment index was not found.");
    return EXIT_FAILURE;
  }

  auto CompilationPos = CompilationIndex->find(Db, Digest);
  if (CompilationPos == CompilationIndex->end(Db)) {
    std::string DigestStr;
    raw_string_ostream SS{DigestStr};
    SS << Digest;
    error("Error: compilation " + SS.str() + " was not found.");
    return EXIT_FAILURE;
  }

  using ELFT = ELF64LE;
  // Use of "naked" new here is a deliberate choice. The various maintained in
  // ELFState<> take measurable time to destroy. This is unnecessary because we
  // can simply leave the OS to release the memory when the process exits.
  auto *const State = new ELFState<ELFT>(Db);
  State->initialize(Db);

  std::array<llvm::Optional<std::vector<std::uint8_t>>,
             static_cast<std::size_t>(pstore::repo::section_kind::last)>
      Prefixes;
  {
    std::vector<OutputSection<ELFT>::SectionInfo> OutputSections;
    OutputSections.resize(::pstore::repo::fragment::member_array::max_size());

    llvm::Optional<pstore::extent<std::uint8_t>> DebugLineHeaderExtent;
    auto Ticket = pstore::repo::compilation::load(Db, CompilationPos->second);

    for (auto const &CM : *Ticket) {
      assert(CM.name != pstore::typed_address<pstore::indirect_string>::null());
      LLVM_DEBUG(dbgs() << "Processing: "
                        << pstore::indirect_string::read(Db, CM.name) << '\n');

      std::fill(std::begin(OutputSections), std::end(OutputSections),
                OutputSection<ELFT>::SectionInfo{});

      pstore::repo::linkage const Linkage = CM.linkage();
      auto const IsLinkOnce = Linkage == pstore::repo::linkage::link_once_any ||
                              Linkage == pstore::repo::linkage::link_once_odr;

      auto const Fragment = pstore::repo::fragment::load(Db, CM.fext);

      if (pstore::repo::debug_line_section const *const DebugLine =
              Fragment->atp<pstore::repo::section_kind::debug_line>()) {
        assert(DebugLine->align() == 1);
        auto &Prefix = Prefixes[static_cast<std::size_t>(
            pstore::repo::section_kind::debug_line)];
        auto Size = std::uint32_t{0};
        pstore::extent<std::uint8_t> const &Ext = DebugLine->header_extent();
        if (!Prefix) {
          assert(!DebugLineHeaderExtent);
          DebugLineHeaderExtent = Ext;
          auto Data = Db.getro(Ext);
          Prefix = std::vector<std::uint8_t>(Data.get(), Data.get() + Ext.size);
          // The header line field value doesn't include the size of the length
          // field itself.
          Size = Ext.size - sizeof(std::uint32_t);
        }
        assert(Ext == *DebugLineHeaderExtent);
        Size += DebugLine->size();
        auto PrefixBytes = Prefix->data();
        std::uint32_t &HeaderLength =
            *reinterpret_cast<std::uint32_t *>(PrefixBytes);
        LLVM_DEBUG(dbgs() << "debug line header field: " << &PrefixBytes
                          << '\n');

        HeaderLength += Size;
      }

      if (Linkage == pstore::repo::linkage::common) {
        auto const Name = pstore::indirect_string::read(Db, CM.name);
        assert(Name.is_in_store());

        if (!Fragment->has_section(pstore::repo::section_kind::bss) ||
            std::count_if(Fragment->begin(), Fragment->end(),
                          pstore::repo::is_target_section) != 1) {
          pstore::shared_sstring_view Owner;
          error("Fragment for common symbol \"" +
                Name.as_string_view(&Owner).to_string() +
                "\" did not contain a sole BSS section");
        }

        pstore::repo::bss_section const &BSS =
            Fragment->at<pstore::repo::section_kind::bss>();
        State->Symbols.insertSymbol(Name, nullptr /*no output section*/,
                                    0 /*offset*/, BSS.size(), Linkage,
                                    BSS.align(), CM.visibility());
        continue;
      }
      // Go through the sections that this fragment contains creating the
      // corresponding ELF section(s) as necessary.
      auto const SectionRange = make_filter_range(
          make_range(std::begin(*Fragment), std::end(*Fragment)),
          pstore::repo::is_target_section);

      for (pstore::repo::section_kind const Section : SectionRange) {
        assert (pstore::repo::is_target_section (Section));

        // The section type and "discriminator" together identify the ELF output
        // section to which this fragment's section data will be appended.
        auto const Discriminator =
            shouldCreateSubsection(IsLinkOnce, Section)
                ? CM.name
                : pstore::typed_address<pstore::indirect_string>::null();
        auto const Id = std::make_tuple(
            getELFSectionType(Section, CM.name, State->Magics), Discriminator);

        decltype(State->Sections)::iterator Pos;
        bool DidInsert;
        std::tie(Pos, DidInsert) = State->Sections.emplace(
            Id, OutputSection<ELFT>(
                    Db, Id, Prefixes[static_cast<std::size_t>(Section)]));

        OutputSection<ELFT> *const OSection = &Pos->second;

        // If this is the first time that we've wanted to append to the ELF
        // section described by 'Id' and the ticket-members has linkonce
        // linkage, then we need to make the section a member of a group
        // section.
        if (DidInsert && IsLinkOnce) {
          decltype(State->Groups)::iterator GroupPos =
              State->Groups.find(CM.name);
          if (GroupPos == State->Groups.end()) {
            bool _;
            std::tie(GroupPos, _) =
                State->Groups.emplace(CM.name, GroupInfo<ELFT>(CM.name));
          }

          GroupPos->second.Members.push_back(OSection);

          // Tell the output section about the group of which it's a member.
          OSection->attachToGroup(&GroupPos->second);
        }

        // Record the location that the later call to section()->append() will
        // assign to this data. We need to account for any alignment padding
        // that that function may place before the data itself (hence the call
        // to alignedContributionSize()).
        OutputSections[static_cast<unsigned>(Section)] =
            OutputSection<ELFT>::SectionInfo(
                OSection,
                OSection->alignedContributionSize(
                    pstore::repo::section_align(*Fragment, Section)));
      }

      // This can't currently be folded into the first loop because it needs the
      // OutputSections array to be built first.

      for (pstore::repo::section_kind Section : SectionRange) {
        OutputSections[static_cast<unsigned>(Section)].section()->append(
            CM, Fragment, Section, State->Symbols, State->Generated,
            OutputSections);
      }
    }
  }

  LLVM_DEBUG(dbgs() << "There are " << State->Groups.size() << " groups\n");

  std::vector<SymbolTable<ELFT>::Value *> OrderedSymbols =
      State->Symbols.sort();

  ELFState<ELFT>::Elf_Ehdr Header;
  State->initELFHeader(Header);
  State->initStandardSections();

  auto &OS = Out->os();
  writeRaw(OS, Header);

  for (auto &S : State->Sections) {
    OutputSection<ELFT> &Section = S.second;
    if (GroupInfo<ELFT> *const Group = Section.group()) {
      State->buildGroupSection(Db, *Group);
    }
    Section.setIndex(State->SectionHeaders.size());
    Section.write(OS, State->Strings, State->Generated,
                  std::back_inserter(State->SectionHeaders));
  }

  State->writeGroupSections(OS);

  // Now do the same for the symbol table.
  {
    auto &S = State->SectionHeaders[SectionIndices::SymTab];
    // st_info should be one greater than the symbol table index of the last
    // local symbol (binding STB_LOCAL).
    S.sh_info = SymbolTable<ELFT>::firstNonLocal(OrderedSymbols);
    bool NeedsExtendedShdnx = false;
    std::tie(S.sh_offset, S.sh_size, NeedsExtendedShdnx) =
        State->Symbols.write(OS, OrderedSymbols);
    if (NeedsExtendedShdnx) {
      // extended symbol table section indices
      size_t const SymtabShndx = State->addSymtabShndxSection();
      auto &SymtabShndxSection = State->SectionHeaders[SymtabShndx];
      std::tie(SymtabShndxSection.sh_offset, SymtabShndxSection.sh_size) =
          State->Symbols.writeSymtabShndx(OS, OrderedSymbols);
    }
  }

  // Write the string table (and patch its section header)
  {
    auto &S = State->SectionHeaders[SectionIndices::StringTab];
    std::tie(S.sh_offset, S.sh_size) = State->Strings.write(OS);
  }

  State->setEShNum(Header);
  Header.e_shoff = State->writeSectionHeaders(OS);
  static_assert(
      static_cast<std::underlying_type_t<SectionIndices>>(
          SectionIndices::StringTab) <
          static_cast<
              std::underlying_type_t<decltype(llvm::ELF::SHN_LORESERVE)>>(
              llvm::ELF::SHN_LORESERVE),
      "String table index should be less than LORESERVE");
  Header.e_shstrndx = SectionIndices::StringTab;

  OS.seek(0);
  writeRaw(OS, Header);

  //  if (Res == 0)
  Out->keep();

  Out->os().flush();

  return EXIT_SUCCESS;
}
