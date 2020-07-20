//===- lib/MC/RepoObjectWriter.cpp - Program Repository Writer-------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements Program Repository file writer information.
//
//===----------------------------------------------------------------------===//

#include "llvm/MC/MCRepoObjectWriter.h"

#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/IR/RepoGlobals.h"
#include "llvm/MC/MCAsmBackend.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCAsmLayout.h"
#include "llvm/MC/MCAssembler.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCFixupKindInfo.h"
#include "llvm/MC/MCObjectFileInfo.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/MC/MCRepoTicketFile.h"
#include "llvm/MC/MCSectionRepo.h"
#include "llvm/MC/MCSymbolRepo.h"
#include "llvm/MC/MCValue.h"
#include "llvm/MC/StringTableBuilder.h"
#include "llvm/Support/Compression.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/Endian.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/StringSaver.h"
#include <set>
#include <string>
#include <unordered_map>
#include <vector>

#include "pstore/core/hamt_map.hpp"
#include "pstore/core/hamt_set.hpp"
#include "pstore/core/index_types.hpp"
#include "pstore/core/sstring_view_archive.hpp"
#include "pstore/core/transaction.hpp"
#include "pstore/mcrepo/compilation.hpp"
#include "pstore/mcrepo/fragment.hpp"
#include "pstore/support/pointee_adaptor.hpp"

using namespace llvm;

#undef DEBUG_TYPE
#define DEBUG_TYPE "repo-object"

namespace {
typedef DenseMap<const MCSectionRepo *, uint32_t> SectionIndexMapTy;

using TransactionType = pstore::transaction<pstore::transaction_lock>;

class RepoObjectWriter : public MCObjectWriter {
private:
  /// The target specific repository writer instance.
  std::unique_ptr<MCRepoObjectTargetWriter> TargetObjectWriter;

  DenseMap<const MCSymbolRepo *, const MCSymbolRepo *> Renames;

  DenseMap<const MCSectionRepo *, std::vector<RepoRelocationEntry>> Relocations;

  // A mapping of a fragment digest to its dependent fragments (saved in the
  // RepoDefinition metadata).
  std::map<repodefinition::DigestType, DenseSet<const RepoDefinition *>>
      Dependents;

  // A mapping of a dependent RepoDefinition to its new symbol name which is
  // used in the string index, compilation member and fragment in the database.
  DenseMap<const RepoDefinition *, std::string> RenamesTN;

  // Note that I don't use StringMap because we take pointers into this
  // structure that must survive insertion.
  // TODO: Compare the performance between std::map and std::unordered_map. If
  // the std::unordered_map operation is faster than std::unordered_map, we
  // should use the std::unordered_map and store the ordered module string set
  // into the database later.
  using ModuleNamesContainer =
      std::map<pstore::raw_sstring_view,
               pstore::typed_address<pstore::indirect_string>>;

  using NamesWithPrefixContainer =
      SmallVector<std::unique_ptr<std::string>, 16>;

  /// A structure of a fragment content.
  struct FragmentContentsType {
    /// Sections contain all the section_contents in this fragment.
    SmallVector<std::unique_ptr<pstore::repo::section_content>, 4> Sections;
    /// Dependents contain all the ticket_members on which this fragment is
    /// dependent.
    SmallVector<pstore::typed_address<pstore::repo::compilation_member>, 4>
        Dependents;
  };

  // A mapping of a fragment digest to its contents (which include the
  // section contents and dependent fragments).
  using ContentsType =
      std::map<repodefinition::DigestType, FragmentContentsType>;

  using TicketType = std::vector<pstore::repo::compilation_member>;
  TicketType CompilationMembers;

  BumpPtrAllocator Alloc;
  StringSaver VersionSymSaver{Alloc};

  // TargetObjectWriter wrappers.
  unsigned getRelocType(MCContext &Ctx, const MCValue &Target,
                        const MCFixup &Fixup, bool IsPCRel) const {
    return TargetObjectWriter->getRelocType(Ctx, Target, Fixup, IsPCRel);
  }

  pstore::index::debug_line_header_index::value_type
  writeDebugLineHeader(TransactionType &Transaction, ContentsType &Fragments);

public:
  RepoObjectWriter(std::unique_ptr<MCRepoObjectTargetWriter> MOTW,
                   raw_pwrite_stream &OS, bool IsLittleEndian)
      : TargetObjectWriter(std::move(MOTW)),
        W(OS, IsLittleEndian ? support::little : support::big) {}

  support::endian::Writer W;

  void reset() override {
    Renames.clear();
    Relocations.clear();
    Dependents.clear();
    RenamesTN.clear();
    MCObjectWriter::reset();
  }

  ~RepoObjectWriter() override;

  void WriteWord(uint64_t Word) { W.write<uint64_t>(Word); }

  template <typename T> void write(T Val) { W.write(Val); }

  void recordRelocation(MCAssembler &Asm, const MCAsmLayout &Layout,
                        const MCFragment *Fragment, const MCFixup &Fixup,
                        MCValue Target, uint64_t &FixedValue) override;

  // Map from a signature symbol to the group section index
  typedef DenseMap<const MCSymbol *, unsigned> RevGroupMapTy;

  void executePostLayoutBinding(MCAssembler &Asm,
                                const MCAsmLayout &Layout) override;

  void writeSectionData(ContentsType &Contents, const MCAssembler &Asm,
                        MCSection &Sec, const MCAsmLayout &Layout,
                        ModuleNamesContainer &Names);

  void buildDependents(ContentsType &Contents, const TicketType &Tickets) const;

  pstore::raw_sstring_view getSymbolName(const MCAssembler &Asm,
                                         const RepoDefinition &TicketMember,
                                         const ModuleNamesContainer &Names,
                                         NamesWithPrefixContainer &Symbols);

  pstore::index::digest buildCompilationRecord(
      const pstore::database &Db, const MCAssembler &Asm,
      ModuleNamesContainer &Names, NamesWithPrefixContainer &Symbols,
      const ContentsType &Fragments, StringRef OutputFile, StringRef Triple);

  static pstore::repo::linkage toPstoreLinkage(GlobalValue::LinkageTypes L);

  static pstore::repo::visibility
  toPstoreVisibility(GlobalValue::VisibilityTypes V);

  bool isSymbolRefDifferenceFullyResolvedImpl(const MCAssembler &Asm,
                                              const MCSymbol &SymA,
                                              const MCFragment &FB, bool InSet,
                                              bool IsPCRel) const override;

  template <typename DispatcherCollectionType>
  static DispatcherCollectionType
  buildFragmentData(const FragmentContentsType &Contents,
                    const pstore::index::debug_line_header_index::value_type
                        &DebugLineHeader);

  static void
  updateDependents(pstore::repo::dependents &Dependent,
                   const pstore::repo::compilation &Compilation,
                   pstore::typed_address<pstore::repo::compilation> addr);

  uint64_t writeObject(MCAssembler &Asm, const MCAsmLayout &Layout) override;
};
} // end anonymous namespace

RepoObjectWriter::~RepoObjectWriter() {}

void RepoObjectWriter::executePostLayoutBinding(MCAssembler &Asm,
                                                const MCAsmLayout &Layout) {
  // Section symbols are used as definitions for undefined symbols with matching
  // names. If there are multiple sections with the same name, the first one is
  // used.
  for (const MCSection &Sec : Asm) {
    const MCSymbol *Begin = Sec.getBeginSymbol();
    if (!Begin)
      continue;

    const MCSymbol *Alias = Asm.getContext().lookupSymbol(Begin->getName());
    if (!Alias || !Alias->isUndefined())
      continue;

    Renames.insert(
        std::make_pair(cast<MCSymbolRepo>(Alias), cast<MCSymbolRepo>(Begin)));
  }
}

void RepoObjectWriter::recordRelocation(MCAssembler &Asm,
                                        const MCAsmLayout &Layout,
                                        const MCFragment *Fragment,
                                        const MCFixup &Fixup, MCValue Target,
                                        uint64_t &FixedValue) {
  MCAsmBackend &Backend = Asm.getBackend();
  bool IsPCRel = Backend.getFixupKindInfo(Fixup.getKind()).Flags &
                 MCFixupKindInfo::FKF_IsPCRel;
  auto const &FixupSection = cast<MCSectionRepo>(*Fragment->getParent());
  uint64_t C = Target.getConstant();
  uint64_t FixupOffset = Layout.getFragmentOffset(Fragment) + Fixup.getOffset();
  MCContext &Ctx = Asm.getContext();

  if (const MCSymbolRefExpr *RefB = Target.getSymB()) {
    assert(RefB->getKind() == MCSymbolRefExpr::VK_None &&
           "Should not have constructed this");

    // Let A, B and C being the components of Target and R be the location of
    // the fixup. If the fixup is not pcrel, we want to compute (A - B + C).
    // If it is pcrel, we want to compute (A - B + C - R).

    // In general, ELF has no relocations for -B. It can only represent (A + C)
    // or (A + C - R). If B = R + K and the relocation is not pcrel, we can
    // replace B to implement it: (A - R - K + C)
    if (IsPCRel) {
      Ctx.reportError(
          Fixup.getLoc(),
          "No fixup available to represent this relative expression");
      return;
    }

    const auto &SymB = cast<MCSymbolRepo>(RefB->getSymbol());

    if (SymB.isUndefined()) {
      Ctx.reportError(Fixup.getLoc(),
                      Twine("symbol '") + SymB.getName() +
                          "' can not be undefined in a subtraction expression");
      return;
    }

    assert(!SymB.isAbsolute() && "Should have been folded");
    const MCSection &SecB = SymB.getSection();
    if (&SecB != &FixupSection) {
      Ctx.reportError(Fixup.getLoc(),
                      "Cannot represent a difference across sections");
      return;
    }

    uint64_t SymBOffset = Layout.getSymbolOffset(SymB);
    uint64_t K = SymBOffset - FixupOffset;
    IsPCRel = true;
    C -= K;
  }

  // We either rejected the fixup or folded B into C at this point.
  const MCSymbolRefExpr *RefA = Target.getSymA();
  const auto *SymA = RefA ? cast<MCSymbolRepo>(&RefA->getSymbol()) : nullptr;

  //bool ViaWeakRef = false;
  if (SymA && SymA->isVariable()) {
    const MCExpr *Expr = SymA->getVariableValue();
    if (const auto *Inner = dyn_cast<MCSymbolRefExpr>(Expr)) {
      if (Inner->getKind() == MCSymbolRefExpr::VK_WEAKREF) {
        SymA = cast<MCSymbolRepo>(&Inner->getSymbol());
        //ViaWeakRef = true; TODO: we're not supporting weak references at the moment.
      }
    }
  }

  unsigned Type = getRelocType(Ctx, Target, Fixup, IsPCRel);
  uint64_t OriginalC = C;
  bool RelocateWithSymbol = false;
  if (!RelocateWithSymbol && SymA && !SymA->isUndefined())
    C += Layout.getSymbolOffset(*SymA);

  uint64_t Addend = C;
  FixedValue = 0;

  const auto *RenamedSymA = SymA;
  if (SymA) {
    if (const MCSymbolRepo *R = Renames.lookup(SymA)) {
      RenamedSymA = R;
    }

    RenamedSymA->setUsedInReloc();
  }

  StringRef UsedSymbolName = RenamedSymA->getName();
  if (auto const *Dependent = SymA->CorrespondingRepoDefinition) {
    Dependents[FixupSection.hash()].insert(Dependent);
    if (GlobalValue::isLocalLinkage(Dependent->getLinkage())) {
      const std::string SymbolName =
          Dependent->getPruned()
              ? Dependent->getNameAsString().str()
              : MCSymbolRepo::getFullName(Ctx, UsedSymbolName,
                                          Dependent->getDigest());
      UsedSymbolName = StringRef(
          RenamesTN.insert(std::make_pair(Dependent, std::move(SymbolName)))
              .first->second);
    }
  }

  Relocations[&FixupSection].emplace_back(
      FixupOffset, RenamedSymA, Type, Addend, SymA, OriginalC, UsedSymbolName);
}

namespace {

pstore::raw_sstring_view stringRefAsView(StringRef S) {
  return {S.data(), S.size()};
}

StringRef stringViewAsRef(pstore::raw_sstring_view S) {
  return {S.data(), S.size()};
}

/// A raw_ostream that writes to an SmallVector or SmallString.  This is a
/// simple adaptor class. This class does not encounter output errors.
/// raw_svector_ostream operates without a buffer, delegating all memory
/// management to the SmallString. Thus the SmallString is always up-to-date,
/// may be used directly and there is no need to call flush().
template <typename Container> class svector_ostream : public raw_pwrite_stream {
public:
  /// Construct a new raw_svector_ostream.
  ///
  /// \param O The vector to write to; this should generally have at least 128
  /// bytes free to avoid any extraneous memory overhead.
  explicit svector_ostream(Container &O) : OS_(O) { SetUnbuffered(); }

  ~svector_ostream() override = default;

  void flush() = delete;

  /// Return a StringRef for the vector contents.
  StringRef str() { return StringRef(OS_.data(), OS_.size()); }

private:
  Container &OS_;

  /// See raw_ostream::write_impl.
  void write_impl(const char *Ptr, size_t Size) override;

  void pwrite_impl(const char *Ptr, size_t Size, uint64_t Offset) override;

  /// Return the current position within the stream.
  uint64_t current_pos() const override;
};

template <typename Container>
uint64_t svector_ostream<Container>::current_pos() const {
  return OS_.size();
}

template <typename Container>
void svector_ostream<Container>::write_impl(const char *Ptr, size_t Size) {
  OS_.append(Ptr, Ptr + Size);
}

template <typename Container>
void svector_ostream<Container>::pwrite_impl(const char *Ptr, size_t Size,
                                             uint64_t Offset) {
  memcpy(OS_.data() + Offset, Ptr, Size);
}

} // namespace

static pstore::repo::section_kind
SectionKindToRepoType(MCSectionRepo const &Section) {
  SectionKind K = Section.getKind();

  if (K.isText()) {
    return pstore::repo::section_kind::text;
  }

  // TODO: add sections types for BSSLocal and BSSExtern?
  if (K.isBSS() || K.isCommon()) {
    return pstore::repo::section_kind::bss;
  }
  if (K.isData()) {
    return pstore::repo::section_kind::data;
  }
  if (K.isMergeableConst4()) {
    return pstore::repo::section_kind::mergeable_const_4;
  }
  if (K.isMergeableConst8()) {
    return pstore::repo::section_kind::mergeable_const_8;
  }
  if (K.isMergeableConst16()) {
    return pstore::repo::section_kind::mergeable_const_16;
  }
  if (K.isMergeableConst32()) {
    return pstore::repo::section_kind::mergeable_const_32;
  }
  assert(!K.isMergeableConst() &&
         "isMergeableConst should be covered by the four previous checks");

  if (K.isMergeable1ByteCString()) {
    return pstore::repo::section_kind::mergeable_1_byte_c_string;
  }
  if (K.isMergeable2ByteCString()) {
    return pstore::repo::section_kind::mergeable_2_byte_c_string;
  }
  if (K.isMergeable4ByteCString()) {
    return pstore::repo::section_kind::mergeable_4_byte_c_string;
  }
  assert(!K.isMergeableCString() &&
         "isMergeableCString should be covered by the three previous checks");

  if (K.isReadOnly()) {
    return pstore::repo::section_kind::read_only;
  }
  if (K.isReadOnlyWithRel()) {
    return pstore::repo::section_kind::rel_ro;
  }
  if (K.isThreadBSS()) {
    return pstore::repo::section_kind::thread_bss;
  }
  if (K.isThreadData()) {
    return pstore::repo::section_kind::thread_data;
  }
  assert(!K.isThreadLocal() &&
         "isThreadLocation should be covered by the two previous checks");

  if (K.isMetadata()) {
    switch (Section.getDebugKind()) {
    case MCSectionRepo::DebugSectionKind::None:
      assert(false);
    case MCSectionRepo::DebugSectionKind::Line:
      return pstore::repo::section_kind::debug_line;
    case MCSectionRepo::DebugSectionKind::String:
      return pstore::repo::section_kind::debug_string;
    case MCSectionRepo::DebugSectionKind::Ranges:
      return pstore::repo::section_kind::debug_ranges;
    }
  }
  llvm_unreachable("Unsupported section type in getRepoSection");
}

void RepoObjectWriter::writeSectionData(ContentsType &Fragments,
                                        const MCAssembler &Asm, MCSection &Sec,
                                        const MCAsmLayout &Layout,
                                        ModuleNamesContainer &Names) {
  auto &Section = static_cast<MCSectionRepo &>(Sec);

  // A "dummy" section is created to provide a default for the assembler
  // or an existing section therefore we don't write it to the repository.
  if (Section.isDummy()) {
    pstore::index::digest Digest{Section.hash().high(), Section.hash().low()};
    LLVM_DEBUG(dbgs() << "A dummy section: section type '"
                      << SectionKindToRepoType(Section)
                      << "' and digest '" << Digest.to_hex_string() << "' \n");

    // The default (dummy) section must have no data, no external/internal
    // fixups.
    // FIXME: repo only supports the debug_line section.
    if (Section.hash() == repodefinition::DigestType{{0}} &&
        (Section.getDebugKind() == MCSectionRepo::DebugSectionKind::Line ||
         Section.getKind().isText())) {
      if (Section.getFragmentList().size() != 1 ||
          Asm.computeFragmentSize(Layout, *Section.begin()) != 0) {
        llvm_unreachable(
            "The default (dummy) section must have no data payload");
      }
      if (Relocations[&Section].size() > 0) {
        llvm_unreachable("The default (dummy) section must have no "
                         "external/internal fixups");
      }
    }

    return;
  }

  pstore::repo::section_kind const St = SectionKindToRepoType(Section);
  assert(Sec.getAlignment() > 0);
  unsigned const Alignment = Sec.getAlignment();

  // TODO: need a cleaner way to check that the alignment value will fit.
  assert(Alignment <= std::numeric_limits<std::uint8_t>::max());

  auto Content = std::make_unique<pstore::repo::section_content>(
      St, static_cast<std::uint8_t>(Alignment));

  // Add the section content to the fragment.
  svector_ostream<decltype(Content->data)> VecOS{Content->data};
  Asm.writeSectionData(VecOS, &Section, Layout);

  auto const &Relocs = Relocations[&Section];
  Content->xfixups.reserve(Relocs.size());
  for (auto const &Relocation : Relocs) {
    using repo_relocation_type = pstore::repo::relocation_type;
    assert (Relocation.Type >= std::numeric_limits <repo_relocation_type>::min ()
            && Relocation.Type <= std::numeric_limits <repo_relocation_type>::max ());

    MCSymbolRepo const *const Symbol = Relocation.Symbol;
    if (Symbol->isInSection()) {
      MCSection &S = Symbol->getSection();
      if (MCSectionRepo const *const TargetSection =
              dyn_cast<MCSectionRepo>(&S)) {
        if (TargetSection->hash() == Section.hash()) {
          Content->ifixups.emplace_back(
              SectionKindToRepoType(*TargetSection),
              static_cast<repo_relocation_type>(Relocation.Type),
              Relocation.Offset, Relocation.Addend);

          continue;
        }
      }
    }

    // Insert the target symbol name into the set of known names for this
    // module. By gathering just a single instance of each string used in this
    // TU we reduce the number of insertions into the global name set (which are
    // performed with the transaction lock held).
    auto It =
        Names
            .emplace(stringRefAsView(Relocation.UsedSymbolName),
                     pstore::typed_address<pstore::indirect_string>::null())
            .first;

    auto NamePtr = reinterpret_cast<std::uintptr_t>(&(*It));

    static_assert(sizeof(NamePtr) <= sizeof(pstore::repo::external_fixup::name),
                  "ExternalFixup::Name is not large enough to hold a pointer");
    assert(Relocation.Type <= std::numeric_limits<decltype(
                                  pstore::repo::external_fixup::type)>::max());

    // Attach a suitable external fixup to this section.
    Content->xfixups.push_back(pstore::repo::external_fixup{
        pstore::typed_address<pstore::indirect_string>(
            pstore::address{NamePtr}),
        static_cast<repo_relocation_type>(Relocation.Type), Relocation.Offset,
        Relocation.Addend});
  }

  LLVM_DEBUG(dbgs() << "section type '" << Content->kind << "' and alignment "
                    << unsigned(Content->align) << '\n');

  Fragments[Section.hash()].Sections.push_back(std::move(Content));
}

void RepoObjectWriter::buildDependents(ContentsType &Fragments,
                                       const TicketType &Tickets) const {
  for (auto const &Dependent : Dependents) {
    auto &D = Fragments[Dependent.first].Dependents;
    for (auto const &TN : Dependent.second) {
      // The corresponding compilation_member lies inside of Tickets.
      assert(TN->CorrespondingCompilationMember >= Tickets.data() &&
             TN->CorrespondingCompilationMember <= &Tickets.back());
      // Record the ticket index in the fragment dependents here. Once the ticket
      // file is stored into the repository,  the fragment dependents are updated
      // from the ticket index to the ticket address in the repository.
      D.push_back(pstore::typed_address<pstore::repo::compilation_member>::make(
          TN->CorrespondingCompilationMember - Tickets.data()));
    }
  }
}

pstore::repo::linkage
RepoObjectWriter::toPstoreLinkage(GlobalValue::LinkageTypes L) {
  switch (L) {
  case GlobalValue::ExternalLinkage:
    return pstore::repo::linkage::external;
  case GlobalValue::LinkOnceAnyLinkage:
    return pstore::repo::linkage::link_once_any;
  case GlobalValue::LinkOnceODRLinkage:
    return pstore::repo::linkage::link_once_odr;
  case GlobalValue::WeakAnyLinkage:
    return pstore::repo::linkage::weak_any;
  case GlobalValue::WeakODRLinkage:
    return pstore::repo::linkage::weak_odr;
  case GlobalValue::PrivateLinkage:
    return pstore::repo::linkage::internal_no_symbol;
  case GlobalValue::InternalLinkage:
    return pstore::repo::linkage::internal;
  case GlobalValue::CommonLinkage:
    return pstore::repo::linkage::common;
  case GlobalValue::AppendingLinkage:
    return pstore::repo::linkage::append;
  default:
    report_fatal_error("Unsupported linkage type");
  }
}

pstore::repo::visibility
RepoObjectWriter::toPstoreVisibility(GlobalValue::VisibilityTypes V) {
  switch (V) {
  case GlobalValue::DefaultVisibility:
    return pstore::repo::visibility::default_vis;
  case GlobalValue::HiddenVisibility:
    return pstore::repo::visibility::hidden_vis;
  case GlobalValue::ProtectedVisibility:
    return pstore::repo::visibility::protected_vis;
  }
  llvm_unreachable("Unsupported visibility type");
}

pstore::raw_sstring_view RepoObjectWriter::getSymbolName(
    const MCAssembler &Asm, const RepoDefinition &TicketMember,
    const ModuleNamesContainer &Names, NamesWithPrefixContainer &Symbols) {

  if (!GlobalValue::isLocalLinkage(TicketMember.getLinkage())) {
    StringRef S = TicketMember.getNameAsString();
    return stringRefAsView(S);
  }

  std::string SymbolName;
  if (TicketMember.getPruned()) {
    SymbolName = TicketMember.getNameAsString().str();
  } else {
    auto RenamesIt = RenamesTN.find(&TicketMember);
    if (RenamesIt == RenamesTN.end()) {
      SymbolName = MCSymbolRepo::getFullName(Asm.getContext(),
                                             TicketMember.getNameAsString(),
                                             repodefinition::NullDigest);
    } else {
      SymbolName = RenamesIt->second;
    }
  }

  auto It = Names.find(stringRefAsView(StringRef(SymbolName)));
  if (It != Names.end())
    return It->first;

  Symbols.push_back(std::make_unique<std::string>(std::move(SymbolName)));
  return pstore::make_sstring_view(*Symbols.back().get());
}

namespace {

/// Returns an active transaction on the pstore database, creating it if
/// not already open.
TransactionType &getRepoTransaction() {
  pstore::database &Repository = llvm::getRepoDatabase();
  static auto Transaction = pstore::begin(Repository);
  return Transaction;
}

raw_ostream &operator<<(raw_ostream &OS, pstore::index::digest const &V) {
  return OS << V.to_hex_string();
}

template <typename StringStorage = SmallString<64>>
StringRef streamPath(raw_fd_ostream &Stream, StringStorage &ResultPath) {
  // Try to get the path from the file descriptor
  std::error_code ErrorCode =
      sys::fs::getPathFromOpenFD(Stream.get_fd(), ResultPath);
  if (ErrorCode) {
    report_fatal_error("RepoDefinition: Invalid output file path: " +
                       ErrorCode.message() + ".");
  }
  llvm::sys::path::remove_filename(ResultPath);
  StringRef OutputFile = ResultPath.str();
  LLVM_DEBUG(dbgs() << "path: " << OutputFile << "\n");
  return OutputFile;
}

template <typename T> ArrayRef<std::uint8_t> makeByteArrayRef(T const &Value) {
  return {reinterpret_cast<std::uint8_t const *>(&Value), sizeof(Value)};
}

} // namespace

pstore::index::digest RepoObjectWriter::buildCompilationRecord(
    const pstore::database &Db, const MCAssembler &Asm,
    ModuleNamesContainer &Names, NamesWithPrefixContainer &Symbols,
    const ContentsType &Fragments, StringRef OutputFile, StringRef Triple) {
  MD5 CompilationHash;

  CompilationHash.update(OutputFile.size());
  CompilationHash.update(OutputFile);

  CompilationHash.update(Triple.size());
  CompilationHash.update(Triple);

  // Only record all compilation members one time.
  DenseSet<RepoDefinition *> SeenCompilationMembers;
  auto Definitions = Asm.getContext().getDefinitions();
  CompilationMembers.reserve(Definitions.size());
  for (const auto Symbol : Definitions) {
    if (!SeenCompilationMembers.insert(Symbol).second)
      continue;
    repodefinition::DigestType const D = Symbol->getDigest();
    // Insert this name into the module-wide string set. This set is later
    // added to the whole-program string set and the ticket name addresses
    // corrected at that time.
    const pstore::raw_sstring_view Name =
        getSymbolName(Asm, *Symbol, Names, Symbols);
    auto It =
        Names
            .emplace(Name,
                     pstore::typed_address<pstore::indirect_string>::null())
            .first;
    // We're storing pointer to the string address into the ticket.
    auto NamePtr = reinterpret_cast<std::uintptr_t>(&(*It));

    auto DigestVal = pstore::index::digest{D.high(), D.low()};
    auto Linkage = toPstoreLinkage(Symbol->getLinkage());
    auto Visibility = toPstoreVisibility(Symbol->getVisibility());
    // If the global object was removed during LLVM's transform passes, this
    // member is not emitted and doesn't insert to the database, and it does
    // not contribute to the hash.
    if (Symbol->getPruned() || Fragments.find(D) != Fragments.end()) {
      CompilationMembers.emplace_back(
          DigestVal, pstore::extent<pstore::repo::fragment>(),
          pstore::typed_address<pstore::indirect_string>(
              pstore::address{NamePtr}),
          Linkage, Visibility);
      // Update the Ticket node to remember the corrresponding ticket member.
      Symbol->CorrespondingCompilationMember = &CompilationMembers.back();
      // If this RepoDefinition was created by the backend, it will be put into
      // dependent list of a fragment. If this fragment is pruned, its dependent
      // definitions will be pruned and contributed to the ticket hash.
      CompilationHash.update(makeByteArrayRef(DigestVal));
      CompilationHash.update(makeByteArrayRef(Linkage));
      CompilationHash.update(makeByteArrayRef(Visibility));
      CompilationHash.update(Name.size());
      CompilationHash.update(stringViewAsRef(Name));
    }
  }

  MD5::MD5Result digest;
  CompilationHash.final(digest);
  return {digest.high(), digest.low()};
}

/// Return true if this ticket is already existing in the database ticket index.
static bool isExistingTicket(const pstore::database &Db,
                             const pstore::index::digest &CompilationDigest) {
  if (auto TicketIndex =
          pstore::index::get_index<pstore::trailer::indices::compilation>(Db,
                                                                     false)) {
    if (TicketIndex->find(Db, CompilationDigest) != TicketIndex->end(Db)) {
      LLVM_DEBUG(dbgs() << "compilation " << CompilationDigest << " exists. skipping\n");
      return true;
    }
  }
  return false;
}

template <typename DispatcherCollectionType>
DispatcherCollectionType RepoObjectWriter::buildFragmentData(
    const FragmentContentsType &Contents,
    const pstore::index::debug_line_header_index::value_type &DebugLineHeader) {
  DispatcherCollectionType Dispatchers;
  Dispatchers.reserve(Contents.Sections.size());

  for (auto const &Content : Contents.Sections) {
    std::unique_ptr<pstore::repo::section_creation_dispatcher> Dispatcher;
    switch (Content->kind) {
    case pstore::repo::section_kind::bss:
      Dispatcher =
          std::make_unique<pstore::repo::bss_section_creation_dispatcher>(
              Content.get());
      break;
    case pstore::repo::section_kind::debug_line:
      // TODO: record the CU's debug line header first, then point this section
      // to it.
      Dispatcher = std::make_unique<
          pstore::repo::debug_line_section_creation_dispatcher>(
          DebugLineHeader.first, DebugLineHeader.second, Content.get());
      break;
    case pstore::repo::section_kind::dependent:
      llvm_unreachable("Invalid section content!");
      break;
    default:
      Dispatcher =
          std::make_unique<pstore::repo::generic_section_creation_dispatcher>(
              Content->kind, Content.get());
    }
    Dispatchers.emplace_back(std::move(Dispatcher));
  }

  if (!Contents.Dependents.empty()) {
    Dispatchers.emplace_back(new pstore::repo::dependents_creation_dispatcher(
        Contents.Dependents.begin(), Contents.Dependents.end()));
  }

  return Dispatchers;
}

void RepoObjectWriter::updateDependents(
    pstore::repo::dependents &Dependent, const pstore::repo::compilation &Compilation,
    pstore::typed_address<pstore::repo::compilation> addr) {
  for (auto &member : Dependent) {
    // Currently, dependent member value is the index in the Ticket.
    auto index = member.absolute();
    assert(index < Compilation.size());
    auto offset = reinterpret_cast<std::uintptr_t>(&Compilation[index]) -
                  reinterpret_cast<std::uintptr_t>(&Compilation);
	// Update the dependent member to record the ticket address.
    member = pstore::typed_address<pstore::repo::compilation_member>::make(
        addr.absolute() + offset);
  }
}

namespace {

pstore::uint128 get_hash_key(ArrayRef<uint8_t> const &arr) {
  MD5 hash;
  hash.update(arr);
  MD5::MD5Result Digest;
  hash.final(Digest);
  return {Digest.high(), Digest.low()};
}

} // end anonymous namespace

pstore::index::debug_line_header_index::value_type
RepoObjectWriter::writeDebugLineHeader(TransactionType &Transaction,
                                       ContentsType &Fragments) {
  auto NullFragmentPos = Fragments.find(repodefinition::NullDigest);
  if (NullFragmentPos != Fragments.end()) {
    auto End = NullFragmentPos->second.Sections.end();
    // TODO: is there a reason why these aren't keyed on the section type?
    auto DebugLinePos = std::find_if(
        NullFragmentPos->second.Sections.begin(), End,
        [](std::unique_ptr<pstore::repo::section_content> const &p) {
          return p->kind == pstore::repo::section_kind::debug_line;
        });
    if (DebugLinePos != End) {
      pstore::repo::section_content const &DebugLine = **DebugLinePos;
      std::size_t const DataSize = DebugLine.data.size();

      // TODO: doing this index search whilst the transaction is open is bad. Do
      // it before the transaction is created and, if not found, add the data
      // and update the index here.
      pstore::database &Db = Transaction.db();
      pstore::uint128 const Key = get_hash_key(
          ArrayRef<uint8_t>(DebugLine.data.begin(), DebugLine.data.end()));
      std::shared_ptr<pstore::index::debug_line_header_index> Index =
          pstore::index::get_index<pstore::trailer::indices::debug_line_header>(
              Db, true /*create*/);
      auto const Pos = Index->find(Db, Key);
      if (Pos != Index->end(Db)) {
        return *Pos;
      }

      // This debug-header wasn't found in the index, so we need to record the
      // data and add it.
      std::pair<std::shared_ptr<void>, pstore::address> Dest =
          Transaction.alloc_rw(DataSize, DebugLine.align);
      std::memcpy(Dest.first.get(), DebugLine.data.data(), DataSize);
      std::memset(Dest.first.get(), 0, 4); // FIXME: 12 in 64-bit DWARF.

      auto const Extent = pstore::make_extent(
          pstore::typed_address<std::uint8_t>(Dest.second), DataSize);
      Index->insert(Transaction, std::make_pair(Key, Extent));
      Fragments.erase(NullFragmentPos);
      return {Key, Extent};
    }
  }

  return {};
}

uint64_t RepoObjectWriter::writeObject(MCAssembler &Asm,
                                       const MCAsmLayout &Layout) {
  uint64_t StartOffset = W.OS.tell();

  ContentsType Fragments;
  ModuleNamesContainer Names;

  SmallString<64> ResultPath;
  StringRef OutputFile =
      streamPath(static_cast<raw_fd_ostream &>(W.OS), ResultPath);
  llvm::Triple const Triple =
      Asm.getContext().getObjectFileInfo()->getTargetTriple();
  std::string const &TripleStr = Triple.str();

  Names.emplace(stringRefAsView(OutputFile),
                pstore::typed_address<pstore::indirect_string>::null());
  Names.emplace(stringRefAsView(TripleStr),
                pstore::typed_address<pstore::indirect_string>::null());

  // Convert the Asm sections to repository fragment sections.
  for (MCSection &Sec : Asm) {
    auto &Section = static_cast<MCSectionRepo &>(Sec);
    writeSectionData(Fragments, Asm, Section, Layout, Names);
  }

  pstore::database &Db = llvm::getRepoDatabase();
  NamesWithPrefixContainer PrefixedNames;
  pstore::index::digest TicketDigest = buildCompilationRecord(
      Db, Asm, Names, PrefixedNames, Fragments, OutputFile, TripleStr);

  buildDependents(Fragments, CompilationMembers);

  pstore::indirect_string_adder NameAdder(Names.size());

  if (!isExistingTicket(Db, TicketDigest)) {
    TransactionType &Transaction = getRepoTransaction();
    {
      std::shared_ptr<pstore::index::compilation_index> const CompilationIndex =
          pstore::index::get_index<pstore::trailer::indices::compilation>(Db);
      assert(CompilationIndex);

      std::shared_ptr<pstore::index::name_index> const NamesIndex =
          pstore::index::get_index<pstore::trailer::indices::name>(Db);
      assert(NamesIndex);

      // Insert the names from this module into the global name set. This loop
      // writes the "indirect_string" records which will become pointers to the
      // real string body. This clusters the pointers together nicely which
      // should help to limit the virtual memory consumption of the
      // repo-linker's store-shadow memory.
      for (ModuleNamesContainer::value_type &NameAddress : Names) {
        LLVM_DEBUG(dbgs() << "insert name: "
                          << stringViewAsRef(NameAddress.first) << '\n');
        pstore::index::name_index::iterator const Pos =
            NameAdder.add(Transaction, NamesIndex, &NameAddress.first).first;
        NameAddress.second =
            pstore::typed_address<pstore::indirect_string>(Pos.get_address());
      }
      // Flush the name bodies.
      NameAdder.flush(Transaction);

      const pstore::index::debug_line_header_index::value_type DebugLineHeader =
          this->writeDebugLineHeader(Transaction, Fragments);

      std::shared_ptr<pstore::index::fragment_index> const FragmentsIndex =
          pstore::index::get_index<pstore::trailer::indices::fragment>(Db);
      assert(FragmentsIndex);

      // TODO: use DenseMap<>.
      std::unordered_map<pstore::index::digest,
                         pstore::extent<pstore::repo::fragment>>
          RepoFragments;

      for (auto &Fragment : Fragments) {
        auto const Key =
            pstore::index::digest{Fragment.first.high(), Fragment.first.low()};

        // The fragment creation APIs require that the input sections are sorted
        // by section_content::type. This guarantees that for them.
        std::sort(Fragment.second.Sections.begin(),
                  Fragment.second.Sections.end(),
                  [](std::unique_ptr<pstore::repo::section_content> const &a,
                     std::unique_ptr<pstore::repo::section_content> const &b) {
                    return a->kind < b->kind;
                  });
        auto SBegin =
            pstore::make_pointee_adaptor(Fragment.second.Sections.begin());
        auto SEnd =
            pstore::make_pointee_adaptor(Fragment.second.Sections.end());

        // The name field of each of the external fixups is pointing into the
        // 'Names' map. Here we turn that into the pstore address of the string.
        std::for_each(SBegin, SEnd, [](pstore::repo::section_content &Section) {
          for (auto &XFixup : Section.xfixups) {
            auto MNC =
                reinterpret_cast<ModuleNamesContainer::value_type const *>(
                    XFixup.name.absolute());
            XFixup.name = MNC->second;
          }
        });

        // Build a collection of dispatchers: one per section in the final
        // fragment. The dispatcher's job is to understand how to construct an
        // individual section instance and write it to the pstore.
        using DispatcherCollection = SmallVector<
            std::unique_ptr<pstore::repo::section_creation_dispatcher>, 4>;
        auto Dispatchers = buildFragmentData<DispatcherCollection>(
            Fragment.second, DebugLineHeader);
        auto Begin = pstore::make_pointee_adaptor(Dispatchers.begin());
        auto End = pstore::make_pointee_adaptor(Dispatchers.end());

        LLVM_DEBUG(dbgs() << "fragment " << Key << " adding. size="
                          << pstore::repo::fragment::size_bytes(Begin, End)
                          << '\n');

        auto const Extent =
            pstore::repo::fragment::alloc(Transaction, Begin, End);
        RepoFragments[Key] = Extent;
        FragmentsIndex->insert(Transaction, std::make_pair(Key, Extent));
      }

      // Find the store address of the output file path.
      auto const OutputFilePos = Names.find(stringRefAsView(OutputFile));
      assert(OutputFilePos != Names.end() && "Output file can't be found!");
      auto OutputPathAddr = OutputFilePos->second;

      // Find the store address of the target triple.
      auto const TriplePos = Names.find(stringRefAsView(TripleStr));
      assert(TriplePos != Names.end() && "Triple can't be found!");
      auto TripleAddr = TriplePos->second;

      // Set the compilation member's grahment extent.
      auto setFragmentExtent =
          [&RepoFragments, &FragmentsIndex,
           &Db](pstore::repo::compilation_member &CompilationMember)
          -> llvm::Optional<pstore::index::fragment_index::const_iterator> {
        if (RepoFragments.find(CompilationMember.digest) !=
            RepoFragments.end()) {
          CompilationMember.fext = RepoFragments[CompilationMember.digest];
          return llvm::None;
        }
        auto It = FragmentsIndex->find(Db, CompilationMember.digest);
        if (It != FragmentsIndex->end(Db)) {
          CompilationMember.fext = It->second;
        }
        return It;
      };

      // The name field of each of ticket_member is pointing into the 'Names'
      // map. Here we turn that into the pstore address of the string.
      for (pstore::repo::compilation_member &CompilationMember :
           CompilationMembers) {
        auto FragmentIndexIt = setFragmentExtent(CompilationMember);

#ifndef NDEBUG
        // Check that we have a fragment for this ticket member's digest
        // value.
        // TODO: remove this check once we're completely confident in the
        // back-end implementation.
        if (!FragmentIndexIt) {
          FragmentIndexIt = FragmentsIndex->find(Db, CompilationMember.digest);
        }
#endif // NDEBUG

        if (FragmentIndexIt &&
            FragmentIndexIt.getValue() == FragmentsIndex->end(Db)) {
          report_fatal_error("The digest of missing repository fragment " +
                             CompilationMember.digest.to_hex_string() +
                             " was found in a compilation member.");
        }

        auto MNC = reinterpret_cast<ModuleNamesContainer::value_type const *>(
            CompilationMember.name.absolute());
        CompilationMember.name = MNC->second;
        LLVM_DEBUG(dbgs() << " compilation member name '"
                          << stringViewAsRef(MNC->first) << "' digest '"
                          << CompilationMember.digest << "' adding." << '\n');
      }

      // Store the Compilation.
      auto CExtent = pstore::repo::compilation::alloc(
          Transaction, OutputPathAddr, TripleAddr, CompilationMembers.begin(),
          CompilationMembers.end());
      CompilationIndex->insert(Transaction, std::make_pair(TicketDigest, CExtent));

      // Update the dependents for each fragment index->address
      for (auto &KV : RepoFragments) {
        std::shared_ptr<pstore::repo::fragment> Fragment =
            pstore::repo::fragment::load(Transaction, KV.second);
        if (auto Dependent =
                Fragment->atp<pstore::repo::section_kind::dependent>()) {
          updateDependents(*Dependent, *pstore::repo::compilation::load(Db, CExtent),
                           CExtent.addr);
        }
      }
    }
    Transaction.commit();
  }

  // write the ticket file itself
  llvm::repo::writeTicketFile(W, TicketDigest);

  return W.OS.tell() - StartOffset;
}

bool RepoObjectWriter::isSymbolRefDifferenceFullyResolvedImpl(
    const MCAssembler &Asm, const MCSymbol &SA, const MCFragment &FB,
    bool InSet, bool IsPCRel) const {

  const auto &SymA = cast<MCSymbolRepo>(SA);
  if (IsPCRel) {
    assert(!InSet);
  }
  return MCObjectWriter::isSymbolRefDifferenceFullyResolvedImpl(Asm, SymA, FB,
                                                                InSet, IsPCRel);
}

std::unique_ptr<MCObjectWriter>
llvm::createRepoObjectWriter(std::unique_ptr<MCRepoObjectTargetWriter> MOTW,
                             raw_pwrite_stream &OS, bool IsLittleEndian) {
  return std::make_unique<RepoObjectWriter>(std::move(MOTW), OS,
                                             IsLittleEndian);
}
