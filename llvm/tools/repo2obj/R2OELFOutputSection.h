//===-- R2OELFOutputSection.h ---------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
#ifndef LLVM_TOOLS_REPO2OBJ_ELFOUTPUTSECTION_H
#define LLVM_TOOLS_REPO2OBJ_ELFOUTPUTSECTION_H

#include "llvm/ADT/Optional.h"
#include "llvm/Object/ELF.h"

#include "pstore/core/sstring_view_archive.hpp"
#include "pstore/mcrepo/compilation.hpp"

#include "R2OELFSectionType.h"
#include "R2OELFStringTable.h"
#include "R2OELFSymbolTable.h"
#include "WriteHelpers.h"

#include <array>
#include <cstdint>
#include <limits>
#include <map>
#include <memory>
#include <tuple>
#include <utility>
#include <vector>

#define DEBUG_TYPE "repo2obj"

using FragmentPtr = std::shared_ptr<pstore::repo::fragment const>;

// The same as the repo types, but with a few extras

namespace details {

struct SectionInfo {
  template <typename StringType>
  SectionInfo(StringType &&N, unsigned T, unsigned F)
      : Name{std::forward<StringType>(N)}, Type{T}, Flags{F} {}
  std::string Name;
  unsigned Type;  ///< sh_type value
  unsigned Flags; ///< sh_flags value
};
// FIXME: this can simply be an array. ELFSectionType is an enum of small
// integers.
using SectionMap = std::map<ELFSectionType, SectionInfo>;
extern SectionMap const SectionAttributes;

} // namespace details

/// Defines the set of standard (fixed) sections that we put in the ELF file.
enum SectionIndices { Null, StringTab, SymTab };

using SectionId =
    std::tuple<ELFSectionType, pstore::typed_address<pstore::indirect_string>>;

template <typename ELFT> class OutputSection;

/// Map from the key-symbol address to the collection of sections belonging to
/// the group.
template <typename ELFT> struct GroupInfo {
  explicit GroupInfo(
      pstore::typed_address<pstore::indirect_string> IdentifyingSymbol_)
      : IdentifyingSymbol{IdentifyingSymbol_} {
    Members.reserve(2U); // the majority of groups have one or two members.
  }
  pstore::typed_address<pstore::indirect_string>
      IdentifyingSymbol; ///< The name that uniquely identifies this group.
  std::vector<OutputSection<ELFT> *> Members;
  std::size_t SectionIndex = 0; ///< The section-index of this group.
};

template <typename ELFT> class OutputSection {
public:
  OutputSection(pstore::database &Db, SectionId Id,
                llvm::Optional<std::vector<uint8_t>> const &Prefix)
      : Db_{Db}, Id_{std::move(Id)}, Prefix_{Prefix} {
    if (Prefix_) {
      SectionSize_ += Prefix_->size();
    }
  }
  OutputSection(OutputSection const &) = delete;
  OutputSection(OutputSection &&) noexcept = default;
  OutputSection &operator=(OutputSection const &) = delete;
  OutputSection &operator=(OutputSection &&) noexcept = default;

  std::uint64_t contributionSize() const { return SectionSize_; }

  /// Returns the amount of data contained by this section adding the specified
  /// alignment to the value.
  std::uint64_t alignedContributionSize(std::uint8_t Align) const {
    return aligned(this->contributionSize(), Align);
  }

  std::uint64_t numRelocations() const { return Relocations_.size(); }
  SectionId const &sectionId() const { return Id_; }

  /// Associates this OutputSection with a specific group section.
  void attachToGroup(GroupInfo<ELFT> *Group) { Group_ = Group; }
  /// Get the Group of which the seciton is a member (if any).
  GroupInfo<ELFT> *group() { return Group_; }

  /// This structure is used to provide the data necessary to create a symbol
  /// which references the first byte of each section contributed by a fragment.
  class SectionInfo {
  public:
    SectionInfo() noexcept = default;
    SectionInfo(OutputSection<ELFT> *Section, std::uint64_t Offset) noexcept
        : Section_{Section}, Offset_{Offset} {}

    OutputSection<ELFT> *section() { return Section_; }
    /// Returns the symbol associated with this section/offset, creating it if
    /// necessary.
    typename SymbolTable<ELFT>::Value *symbol(SymbolTable<ELFT> &Symbols,
                                              GeneratedNames &Generated);

  private:
    OutputSection<ELFT> *Section_ = nullptr;
    std::uint64_t Offset_ = 0;
    typename SymbolTable<ELFT>::Value *Symbol_ = nullptr;
  };

  void append(pstore::repo::compilation_member const &CM,
              FragmentPtr FragmentData, pstore::repo::section_kind SectionKind,
              SymbolTable<ELFT> &Symbols, GeneratedNames &Generated,
              std::vector<SectionInfo> &OutputSections);

  /// \returns The number of ELF sections added by this OutputSection.
  std::size_t numSections() const;

  /// Writes the section data, relocations, and creates the necessary section
  /// header table entries.
  ///
  /// \tparam OutputIt  An output iterator to which will be written one or more
  /// instance of the Elf_Shdr type.
  /// \param OS The binary output stream to which the contents of the output
  /// sections are written.
  /// \param SectionNames  The collection of section names.
  /// \param Generated  The object managing names created during object-file
  /// generation.
  /// \param OutShdr  An output iterator to which will be written
  /// one or more instance of the Elf_Shdr type as the write function creates
  /// sections in the output.
  template <typename OutputIt>
  OutputIt write(llvm::raw_ostream &OS, StringTable &SectionNames,
                 GeneratedNames &Generated, OutputIt OutShdr) const;

  ELFSectionType getType() const { return std::get<0>(Id_); }
  size_t getIndex() const {
    assert(Index_ != UnknownIndex);
    return Index_;
  }
  void setIndex(size_t Index) {
    assert(Index != UnknownIndex && Index_ == UnknownIndex);
    Index_ = Index;
  }

private:
  pstore::database const &Db_;
  SectionId const Id_;
  llvm::Optional<std::vector<uint8_t>> const &Prefix_;
  GroupInfo<ELFT> *Group_ = nullptr;

  static constexpr auto UnknownIndex = std::numeric_limits<size_t>::max();
  size_t Index_ = UnknownIndex; // The section header table index
  uint8_t Align_ = 0;

  // TODO: We have no a priori knowledge of the number of text contributions to
  // this output section. Using some sort of "chunked vector" might be
  // considerably more efficient.
  std::vector<std::pair<FragmentPtr, pstore::repo::section_kind>>
      Contributions_;
  /// The number of data bytes contained in this section.
  std::uint64_t SectionSize_ = 0;

  using Elf_Word = typename llvm::object::ELFFile<ELFT>::Elf_Word;
  using Elf_Rela = typename llvm::object::ELFFile<ELFT>::Elf_Rela;
  using Elf_Shdr = typename llvm::object::ELFFile<ELFT>::Elf_Shdr;
  using SymbolTarget = typename SymbolTable<ELFT>::SymbolTarget;

  struct Relocation {
    Relocation(typename SymbolTable<ELFT>::Value *Symbol_,
               pstore::repo::relocation_type Type_, std::uint64_t Offset_,
               std::uint64_t Addend_)
        : Symbol{Symbol_}, Type{Type_}, Offset{Offset_}, Addend{Addend_} {
      assert(Symbol != nullptr);
    }

    /// The symbol targeted by this relocation.
    typename SymbolTable<ELFT>::Value *Symbol;
    pstore::repo::relocation_type Type;
    std::uint64_t Offset;
    std::uint64_t Addend;
  };
  std::vector<Relocation> Relocations_;

  /// \returns True if the input value is a power of 2.
  template <typename Ty,
            typename = typename std::enable_if<std::is_unsigned<Ty>::value>>
  static inline bool isPowerOfTwo(Ty N) {
    //  if a number n is a power of 2 then bitwise & of n and n-1 will be zero.
    return N && !(N & (N - 1U));
  }

  /// \param v  The value to be aligned.
  /// \param align  The alignment required for 'v'.
  /// \returns  The value closest to but greater than or equal to 'V' for which
  /// v modulo align is zero.
  template <typename IntType>
  static inline IntType aligned(IntType V, std::size_t Align) {
    assert(isPowerOfTwo(Align));
    return (V + Align - 1U) & ~(Align - 1U);
  }

  void writePadding(llvm::raw_ostream &OS, std::uint64_t Bytes) const;
  void writeNopData(llvm::raw_ostream &OS, std::uint64_t Count) const;

  std::string dataSectionName(
      std::string SectionName,
      pstore::typed_address<pstore::indirect_string> DiscriminatorName) const;
  std::string relocationSectionName(std::string const &BaseName) const;
};

// FIXME: this needs to be passeed in and not hard-wired.
constexpr bool IsMips64EL = false;

template <typename ELFT>
typename SymbolTable<ELFT>::Value *
OutputSection<ELFT>::SectionInfo::symbol(SymbolTable<ELFT> &Symbols,
                                         GeneratedNames &Generated) {
  using namespace llvm;
  if (Symbol_ == nullptr) {
    // FIXME: a static local isn't a great way to implement this. I just need to
    // be able to generate a unique name.
    static auto PrivateSymbolCount = 0U;

    auto Name = Generated.add(".LR" + std::to_string(PrivateSymbolCount++));
    Symbol_ = Symbols.insertSymbol(
        Name, Section_, Offset_, 0 /*size*/, pstore::repo::linkage::internal,
        1U /*alignment*/, pstore::repo::visibility::default_vis);

    LLVM_DEBUG(dbgs() << "  created symbol:" << Name
                      << " for internal fixup (offset:" << Offset_
                      << " contributionSize:" << Section_->contributionSize()
                      << ")\n");

    assert(Symbol_ != nullptr);
  }
  return Symbol_;
}

// FIXME: this code is ripped from X86AsmBackend::writeNopData(). Unfortunately
// that function is somewhat coupled to its surrounding classes fairly tightly
// so that it's easier in the short term to replicate it here. Refactor.
/// \brief Write a sequence of optimal nops to the output, covering \p Count
/// bytes.
template <typename ELFT>
void OutputSection<ELFT>::writeNopData(llvm::raw_ostream &OS,
                                       uint64_t Count) const {
  static const uint8_t Nops[10][10] = {
      // nop
      {0x90},
      // xchg %ax,%ax
      {0x66, 0x90},
      // nopl (%[re]ax)
      {0x0f, 0x1f, 0x00},
      // nopl 0(%[re]ax)
      {0x0f, 0x1f, 0x40, 0x00},
      // nopl 0(%[re]ax,%[re]ax,1)
      {0x0f, 0x1f, 0x44, 0x00, 0x00},
      // nopw 0(%[re]ax,%[re]ax,1)
      {0x66, 0x0f, 0x1f, 0x44, 0x00, 0x00},
      // nopl 0L(%[re]ax)
      {0x0f, 0x1f, 0x80, 0x00, 0x00, 0x00, 0x00},
      // nopl 0L(%[re]ax,%[re]ax,1)
      {0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00},
      // nopw 0L(%[re]ax,%[re]ax,1)
      {0x66, 0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00},
      // nopw %cs:0L(%[re]ax,%[re]ax,1)
      {0x66, 0x2e, 0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00},
  };

#if 0
  // This CPU doesn't support long nops. If needed add more.
  // FIXME: Can we get this from the subtarget somehow?
  // FIXME: We could generate something better than plain 0x90.
  if (!HasNopl) {
    for (uint64_t i = 0; i < Count; ++i)
      OW->write8(0x90);
    return true;
  }
#endif

  // 15 is the longest single nop instruction.  Emit as many 15-byte nops as
  // needed, then emit a nop of the remaining length.
  auto MaxNopLength = uint64_t{15};
  do {
    auto const ThisNopLength =
        static_cast<std::uint8_t>(std::min(Count, MaxNopLength));
    uint8_t const Prefixes = ThisNopLength <= 10 ? 0 : ThisNopLength - 10;
    for (uint8_t i = 0; i < Prefixes; i++) {
      write8(OS, 0x66);
    }
    const uint8_t Rest = ThisNopLength - Prefixes;
    for (uint8_t i = 0; i < Rest; i++) {
      write8(OS, Nops[Rest - 1][i]);
    }
    Count -= ThisNopLength;
  } while (Count != 0);
}

// writePadding
// ~~~~~~~~~~~~
template <typename ELFT>
void OutputSection<ELFT>::writePadding(llvm::raw_ostream &OS,
                                       std::uint64_t Bytes) const {
  if (std::get<0>(sectionId()) == ELFSectionType::text) {
    return writeNopData(OS, Bytes);
  }
  for (auto Ctr = std::uint64_t{0}; Ctr < Bytes; ++Ctr) {
    write8(OS, 0);
  }
}

// append
// ~~~~~~
template <typename ELFT>
void OutputSection<ELFT>::append(pstore::repo::compilation_member const &CM,
                                 FragmentPtr Fragment,
                                 pstore::repo::section_kind SectionKind,
                                 SymbolTable<ELFT> &Symbols,
                                 GeneratedNames &Generated,
                                 std::vector<SectionInfo> &OutputSections) {
  using namespace llvm;

  Contributions_.emplace_back(Fragment, SectionKind);

  auto const ObjectSize =
      pstore::repo::section_size(*Fragment, SectionKind);
  LLVM_DEBUG(dbgs() << "  generating relocations FROM '"
                    << pstore::indirect_string::read(Db_, CM.name) << "'\n");

  std::uint8_t const DataAlign =
      pstore::repo::section_align(*Fragment, SectionKind);
  // ELF section alignment is the maximum of the alignment of all its
  // contributions.
  Align_ = std::max(Align_, DataAlign);

  SectionSize_ = aligned(SectionSize_, DataAlign);

  // "append" linkage is slightly unusual in that multiple definitions of the
  // same symbol simply pile up one after the other in the output. ELF doesn't
  // have this concept, obviously, so we need to ensure that we don't produce a
  // definition of this symbol in each object file that will result in an error
  // when they are linked.
  //
  // This check is sufficient for the llvm.globl_ctors/dtors use-case where the
  // symbols are mapped to the .init_array/.fini_array sections and we don't
  // actually need a symbol which references the data.

  if (CM.linkage() != pstore::repo::linkage::append) {
    Symbols.insertSymbol(pstore::indirect_string::read(Db_, CM.name), this,
                         SectionSize_, ObjectSize, CM.linkage(), DataAlign,
                         CM.visibility());
  }

  for (pstore::repo::external_fixup const &XFixup :
       pstore::repo::section_xfixups(*Fragment, SectionKind)) {
    auto const TargetName = pstore::indirect_string::read(Db_, XFixup.name);
    LLVM_DEBUG(dbgs() << "  generating relocation TO '" << TargetName << "'\n");
    Relocations_.emplace_back(Symbols.insertSymbol(TargetName, XFixup.type),
                              XFixup.type, XFixup.offset + SectionSize_,
                              XFixup.addend);
  }

  for (pstore::repo::internal_fixup const &IFixup :
       pstore::repo::section_ifixups(*Fragment, SectionKind)) {
    // "this" and "patch offset" together define the address that the fixup will
    // modify.
    auto const PatchOffset = SectionSize_ + IFixup.offset;
    LLVM_DEBUG(llvm::dbgs()
               << "patch section is " << std::get<0>(this->sectionId()) << " + "
               << PatchOffset << '\n');

    // "target section" and friends define the value that the fixup will write
    // to the "patch address".
    auto const TargetSectionIndex = static_cast<
        typename std::underlying_type<decltype(IFixup.section)>::type>(
        IFixup.section);
    assert(TargetSectionIndex >= 0 &&
           TargetSectionIndex < OutputSections.size());
    SectionInfo &TargetSection = OutputSections[TargetSectionIndex];

    LLVM_DEBUG(llvm::dbgs()
               << "reloc target section is "
               << std::get<0>(TargetSection.section()->sectionId()) << '\n');

    Relocations_.emplace_back(TargetSection.symbol(Symbols, Generated),
                              IFixup.type, PatchOffset, IFixup.addend);
  }

  SectionSize_ += ObjectSize;
}

// numSections
// ~~~~~~~~~~~
template <typename ELFT> std::size_t OutputSection<ELFT>::numSections() const {
  return 1U + static_cast<std::size_t>(Relocations_.size() > 0);
}

// write
// ~~~~~
template <typename ELFT>
template <typename OutputIt>
OutputIt
OutputSection<ELFT>::write(llvm::raw_ostream &OS, StringTable &SectionNames,
                           GeneratedNames &Generated, OutputIt OutShdr) const {
  assert(Index_ != UnknownIndex);
  auto const GroupFlag =
      Group_ != nullptr ? Elf_Word{llvm::ELF::SHF_GROUP} : Elf_Word{0};

  // Ensure that the start of the section data is suitably aligned.
  auto Pos = OS.tell();
  auto const StartPos = aligned(Pos, Align_);
  assert(StartPos >= Pos);
  this->writePadding(OS, StartPos - Pos);

  Pos = StartPos;

  if (Prefix_) {
    OS.write(reinterpret_cast<char const *>(Prefix_->data()), Prefix_->size());
    Pos += Prefix_->size();
  }

  for (auto const &Contribution : Contributions_) {
    auto Fragment = Contribution.first;
    auto Type = Contribution.second;

    pstore::repo::container<std::uint8_t> D =
        pstore::repo::section_value(*Fragment, Type);

    assert(Align_ >= pstore::repo::section_align(*Fragment, Type));
    auto const AlignedPos = aligned(Pos, pstore::repo::section_align(*Fragment, Type));
    assert(AlignedPos >= Pos);
    this->writePadding(OS, AlignedPos - Pos);
    std::size_t const Size = D.size();
    OS.write(reinterpret_cast<char const *>(D.data()), Size);
    Pos = AlignedPos + Size;
  }
  assert(Pos == OS.tell());
  auto const SectionType = this->getType();
  assert(OS.tell() - StartPos == (SectionType == ELFSectionType::bss)
             ? 0
             : SectionSize_);

  auto const &Attrs = details::SectionAttributes.find(SectionType);
  assert(Attrs != details::SectionAttributes.end());
  std::string const SectionName = this->dataSectionName(
      Attrs->second.Name, std::get<1>(Id_) /*Discriminator*/);
  {
    LLVM_DEBUG(llvm::dbgs()
               << "section " << SectionName << " index " << Index_ << '\n');

    Elf_Shdr SH;
    zero(SH);
    SH.sh_name = SectionNames.insert(Generated.add(SectionName));
    SH.sh_type = Attrs->second.Type;
    SH.sh_flags = Attrs->second.Flags | GroupFlag;
    SH.sh_offset = StartPos;
    SH.sh_size = SectionSize_;
    SH.sh_addralign = Align_;
    *(OutShdr++) = SH;
  }

  if (Relocations_.size() > 0) {
    writeAlignmentPadding<Elf_Rela>(OS);
    auto const RelaStartPos = OS.tell();

    for (Relocation const &R : Relocations_) {
      Elf_Rela Rel;
      auto SymbolIndex = R.Symbol->Index;
      Rel.setSymbolAndType(SymbolIndex, R.Type, IsMips64EL);
      Rel.r_offset = R.Offset;
      Rel.r_addend = R.Addend;
      OS.write(reinterpret_cast<char const *>(&Rel), sizeof(Rel));
    }

    Elf_Shdr RelaSH;
    zero(RelaSH);
    RelaSH.sh_name = SectionNames.insert(
        Generated.add(this->relocationSectionName(SectionName)));
    RelaSH.sh_type = llvm::ELF::SHT_RELA;
    RelaSH.sh_flags = llvm::ELF::SHF_INFO_LINK |
                      GroupFlag; // sh_info holds index of the target section.
    RelaSH.sh_offset = RelaStartPos;
    RelaSH.sh_size = Relocations_.size() * sizeof(Elf_Rela);
    RelaSH.sh_link = SectionIndices::SymTab;
    RelaSH.sh_info = Index_; // target section
    RelaSH.sh_entsize = sizeof(Elf_Rela);
    RelaSH.sh_addralign = alignof(Elf_Rela);
    *(OutShdr++) = RelaSH;
  }
  return OutShdr;
}

// dataSectionName
// ~~~~~~~~~~~~~~~
template <typename ELFT>
std::string OutputSection<ELFT>::dataSectionName(
    std::string SectionName,
    pstore::typed_address<pstore::indirect_string> DiscriminatorName) const {
  if (DiscriminatorName !=
      pstore::typed_address<pstore::indirect_string>::null()) {
    SectionName += '.';
    SectionName +=
        pstore::indirect_string::read(Db_, DiscriminatorName).to_string();
  }
  return SectionName;
}

// relocationSectionName
// ~~~~~~~~~~~~~~~~~~~~~
template <typename ELFT>
std::string
OutputSection<ELFT>::relocationSectionName(std::string const &BaseName) const {
  if (BaseName[0] == '.') {
    return std::string{".rela"} + BaseName;
  }
  return std::string{".rela."} + BaseName;
}

#endif // LLVM_TOOLS_REPO2OBJ_ELFOUTPUTSECTION_H
