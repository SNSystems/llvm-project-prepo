//===- lib/MC/RepoObjectWriter.cpp - Program Repository Writer-------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements Program Repository file writer information.
//
//===----------------------------------------------------------------------===//

#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallString.h"
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
#include "llvm/MC/MCRepoObjectWriter.h"
#include "llvm/MC/MCRepoTicketFile.h"
#include "llvm/MC/MCSectionRepo.h"
#include "llvm/MC/MCSymbolRepo.h"
#include "llvm/MC/MCValue.h"
#include "llvm/MC/StringTableBuilder.h"
#include "llvm/Support/Compression.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/StringSaver.h"
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

using TransactionType = pstore::transaction<pstore::transaction_lock>;

class RepoObjectWriter : public MCObjectWriter {
private:
  /// The target specific repository writer instance.
  std::unique_ptr<MCRepoObjectTargetWriter> TargetObjectWriter;

  DenseMap<const MCSymbolRepo *, const MCSymbolRepo *> Renames;

  DenseMap<const MCSectionRepo *, std::vector<RepoRelocationEntry>> Relocations;

  // A mapping of a fragment digest to its linked definitions (saved in the
  // RepoDefinition metadata).
  std::map<repodefinition::DigestType, DenseSet<const RepoDefinition *>>
      LinkedDefinitions;

  // A mapping of a linked definition RepoDefinition to its new symbol name.
  DenseMap<const RepoDefinition *, std::string> RenamesTN;

  // Note that I don't use StringMap because we take pointers into this
  // structure that must survive insertion.
  using ModuleNamesContainer =
      std::map<pstore::raw_sstring_view,
               pstore::typed_address<pstore::indirect_string>>;

  using NamesWithPrefixContainer =
      SmallVector<std::unique_ptr<std::string>, 16>;

  struct FragmentContentsType {
    /// Sections contain all the section_contents in this fragment.
    SmallVector<std::unique_ptr<pstore::repo::section_content>, 4> Sections;
    /// The definitions which must be present in a compilation contining this
    /// fragment.
    SmallVector<pstore::repo::linked_definitions::value_type, 4>
        LinkedDefinitions;
  };

  // A mapping of a fragment digest to its contents (which include the
  // section contents and linked definitions).
  using ContentsType =
      std::map<repodefinition::DigestType, FragmentContentsType>;

  using DefinitionContainer = std::vector<pstore::repo::definition>;
  DefinitionContainer CompilationDefinitions;

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
    LinkedDefinitions.clear();
    RenamesTN.clear();
    MCObjectWriter::reset();
  }

  void WriteWord(uint64_t Word) { W.write<uint64_t>(Word); }

  template <typename T> void write(T Val) { W.write(Val); }

  void recordRelocation(MCAssembler &Asm, const MCAsmLayout &Layout,
                        const MCFragment *Fragment, const MCFixup &Fixup,
                        MCValue Target, uint64_t &FixedValue) override;

  void executePostLayoutBinding(MCAssembler &Asm,
                                const MCAsmLayout &Layout) override;

  void writeSectionData(ContentsType &Contents, const MCAssembler &Asm,
                        MCSection &Sec, const MCAsmLayout &Layout,
                        ModuleNamesContainer &Names);

  void buildLinkedDefinitions(ContentsType &Contents,
                              pstore::index::digest CompilationDigest,
                              const DefinitionContainer &Definitions) const;

  pstore::raw_sstring_view getSymbolName(const MCAssembler &Asm,
                                         const RepoDefinition &TicketMember,
                                         const ModuleNamesContainer &Names,
                                         NamesWithPrefixContainer &Symbols);

  pstore::index::digest
  buildCompilationRecord(const pstore::database &Db, const MCAssembler &Asm,
                         ModuleNamesContainer &Names,
                         NamesWithPrefixContainer &Symbols,
                         const ContentsType &Fragments, StringRef Triple);

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

  static void updateLinkedDefinitions(
      pstore::repo::linked_definitions &LinkedDefinitions,
      const pstore::repo::compilation &Compilation,
      pstore::typed_address<pstore::repo::compilation> Addr);

  uint64_t writeObject(MCAssembler &Asm, const MCAsmLayout &Layout) override;
};
} // end anonymous namespace

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
  const auto &FixupSection = cast<MCSectionRepo>(*Fragment->getParent());
  uint64_t C = Target.getConstant();
  uint64_t FixupOffset = Layout.getFragmentOffset(Fragment) + Fixup.getOffset();
  MCContext &Ctx = Asm.getContext();

  if (const MCSymbolRefExpr *RefB = Target.getSymB()) {
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

    assert(!IsPCRel && "should have been folded");
    IsPCRel = true;
    C += FixupOffset - Layout.getSymbolOffset(SymB);
  }

  // We either rejected the fixup or folded B into C at this point.
  const MCSymbolRefExpr *RefA = Target.getSymA();
  const auto *SymA = RefA ? cast<MCSymbolRepo>(&RefA->getSymbol()) : nullptr;

  // bool ViaWeakRef = false;
  if (SymA && SymA->isVariable()) {
    const MCExpr *Expr = SymA->getVariableValue();
    if (const auto *Inner = dyn_cast<MCSymbolRefExpr>(Expr)) {
      if (Inner->getKind() == MCSymbolRefExpr::VK_WEAKREF) {
        SymA = cast<MCSymbolRepo>(&Inner->getSymbol());
        // ViaWeakRef = true; TODO: we're not supporting weak references at the
        // moment.
      }
    }
  }

  unsigned Type = TargetObjectWriter->getRelocType(Ctx, Target, Fixup, IsPCRel);
  bool RelocateWithSymbol = false;
  FixedValue = !RelocateWithSymbol && SymA && !SymA->isUndefined()
                   ? C + Layout.getSymbolOffset(*SymA)
                   : C;

  uint64_t Addend = FixedValue;
  FixedValue = 0;

  const MCSymbolRepo *RenamedSymA = SymA;
  if (SymA) {
    if (const MCSymbolRepo *R = Renames.lookup(SymA)) {
      RenamedSymA = R;
    }

    RenamedSymA->setUsedInReloc();
  }

  StringRef UsedSymbolName = RenamedSymA->getName();
  if (const RepoDefinition *const Definition =
          SymA->CorrespondingRepoDefinition) {
    LinkedDefinitions[FixupSection.hash()].insert(Definition);
    if (GlobalValue::isLocalLinkage(Definition->getLinkage())) {
      const std::string SymbolName =
          Definition->getPruned()
              ? Definition->getNameAsString().str()
              : MCSymbolRepo::getFullName(Ctx, UsedSymbolName,
                                          Definition->getDigest());
      UsedSymbolName = StringRef(
          RenamesTN.insert(std::make_pair(Definition, std::move(SymbolName)))
              .first->second);
    }
  }

  Relocations[&FixupSection].emplace_back(FixupOffset, RenamedSymA, Type,
                                          static_cast<std::int64_t>(Addend),
                                          SymA, C, UsedSymbolName);
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
sectionKindToRepoType(MCSectionRepo const &Section) {
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
      llvm_unreachable("Unsupported debug section in SectionKindToRepoType");
    case MCSectionRepo::DebugSectionKind::Line:
      return pstore::repo::section_kind::debug_line;
    case MCSectionRepo::DebugSectionKind::String:
      return pstore::repo::section_kind::debug_string;
    case MCSectionRepo::DebugSectionKind::Ranges:
      return pstore::repo::section_kind::debug_ranges;
    case MCSectionRepo::DebugSectionKind::Loc:
      return pstore::repo::section_kind::debug_loc;
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
                      << sectionKindToRepoType(Section) << "' and digest '"
                      << Digest.to_hex_string() << "' \n");

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

  pstore::repo::section_kind const St = sectionKindToRepoType(Section);
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
    assert(Relocation.Type >=
               std::numeric_limits<repo_relocation_type>::min() &&
           Relocation.Type <= std::numeric_limits<repo_relocation_type>::max());

    MCSymbolRepo const *const Symbol = Relocation.Symbol;
    if (Symbol->isInSection()) {
      MCSection &S = Symbol->getSection();
      if (MCSectionRepo const *const TargetSection =
              dyn_cast<MCSectionRepo>(&S)) {
        if (TargetSection->hash() == Section.hash()) {
          Content->ifixups.emplace_back(
              sectionKindToRepoType(*TargetSection),
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
    bool IsWeak = Symbol->isExternalWeak();
    Content->xfixups.push_back(pstore::repo::external_fixup{
        pstore::typed_address<pstore::indirect_string>(
            pstore::address{NamePtr}),
        static_cast<repo_relocation_type>(Relocation.Type),
        IsWeak ? pstore::repo::binding::weak : pstore::repo::binding::strong,
        Relocation.Offset, Relocation.Addend});

    if (IsWeak) {
      pstore::index::digest Digest{Section.hash().high(), Section.hash().low()};
      LLVM_DEBUG(dbgs() << "fragment " << Digest.to_hex_string()
                        << " has an xfixup to an external weak symbol '"
                        << Symbol->getName() << "' \n");
    }
  }

  LLVM_DEBUG(dbgs() << "section type '" << Content->kind << "' and alignment "
                    << unsigned(Content->align) << '\n');

  Fragments[Section.hash()].Sections.push_back(std::move(Content));
}

void RepoObjectWriter::buildLinkedDefinitions(
    ContentsType &Fragments, const pstore::index::digest CompilationDigest,
    const DefinitionContainer &Definitions) const {
  for (auto const &LD : LinkedDefinitions) {
    auto &D = Fragments[LD.first].LinkedDefinitions;
    for (auto const &TN : LD.second) {
      assert(TN->CorrespondingDefinition >= Definitions.data() &&
             TN->CorrespondingDefinition <= &Definitions.back() &&
             "The corresponding definition must lie within the "
             "Definitions container.");

      // Record the compilation index in the linked definitions record here.
      // Once the compilation is stored in the repository and we know its
      // address, this value is updated to reflect the actual address of the
      // definition.
      const auto Index = TN->CorrespondingDefinition - Definitions.data();
      D.emplace_back(
          CompilationDigest, Index,
          pstore::typed_address<pstore::repo::definition>::make(Index));
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

#ifndef NDEBUG
raw_ostream &operator<<(raw_ostream &OS, pstore::index::digest const &V) {
  return OS << V.to_hex_string();
}
#endif

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
    const ContentsType &Fragments, StringRef Triple) {
  MD5 CompilationHash;

  CompilationHash.update(Triple.size());
  CompilationHash.update(Triple);

  DenseSet<RepoDefinition *> SeenDefinitions;
  auto Definitions = Asm.getContext().getDefinitions();
  CompilationDefinitions.reserve(Definitions.size());
  for (const auto Symbol : Definitions) {
    if (!SeenDefinitions.insert(Symbol).second)
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
      CompilationDefinitions.emplace_back(
          DigestVal, pstore::extent<pstore::repo::fragment>(),
          pstore::typed_address<pstore::indirect_string>(
              pstore::address{NamePtr}),
          Linkage, Visibility);
      Symbol->CorrespondingDefinition = &CompilationDefinitions.back();
      // If this RepoDefinition was created by the backend, it will be put into
      // the linked-definitions section of a fragment. If this fragment is
      // pruned, its linked-definitions will be pruned and contribute to the
      // compilation hash.
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
          pstore::index::get_index<pstore::trailer::indices::compilation>(
              Db, false)) {
    if (TicketIndex->find(Db, CompilationDigest) != TicketIndex->end(Db)) {
      LLVM_DEBUG(dbgs() << "compilation " << CompilationDigest
                        << " exists. skipping\n");
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
    case pstore::repo::section_kind::linked_definitions:
      llvm_unreachable("Invalid section content!");
      break;
    default:
      Dispatcher =
          std::make_unique<pstore::repo::generic_section_creation_dispatcher>(
              Content->kind, Content.get());
    }
    Dispatchers.emplace_back(std::move(Dispatcher));
  }

  if (!Contents.LinkedDefinitions.empty()) {
    Dispatchers.emplace_back(
        new pstore::repo::linked_definitions_creation_dispatcher(
            Contents.LinkedDefinitions.begin(),
            Contents.LinkedDefinitions.end()));
  }

  return Dispatchers;
}

// Update the linked definition members to record the definition address.
void RepoObjectWriter::updateLinkedDefinitions(
    pstore::repo::linked_definitions &LinkedDefinitions,
    const pstore::repo::compilation &Compilation,
    pstore::typed_address<pstore::repo::compilation> Addr) {
  (void)Compilation;

  for (auto &Member : LinkedDefinitions) {
    assert(Member.index < Compilation.size());
    Member.pointer =
        pstore::repo::compilation::index_address(Addr, Member.index);
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
      pstore::uint128 const Key =
          get_hash_key(ArrayRef<uint8_t>{DebugLine.data.data(), DataSize});
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
  Expected<llvm::SmallString<256>> OutputDir =
      llvm::mc::repo::realTicketDirectory(OutputFile);
  consumeError(OutputDir.takeError()); // What else can we do here?

  llvm::Triple const Triple =
      Asm.getContext().getObjectFileInfo()->getTargetTriple();
  std::string const &TripleStr = Triple.str();
  Names.emplace(stringRefAsView(TripleStr),
                pstore::typed_address<pstore::indirect_string>::null());

  // Convert the Asm sections to repository fragment sections.
  for (MCSection &Sec : Asm) {
    auto &Section = static_cast<MCSectionRepo &>(Sec);
    writeSectionData(Fragments, Asm, Section, Layout, Names);
  }

  pstore::database &Db = llvm::getRepoDatabase();
  NamesWithPrefixContainer PrefixedNames;
  const pstore::index::digest CompilationDigest = buildCompilationRecord(
      Db, Asm, Names, PrefixedNames, Fragments, TripleStr);

  buildLinkedDefinitions(Fragments, CompilationDigest, CompilationDefinitions);

  pstore::indirect_string_adder NameAdder(Names.size());

  if (!isExistingTicket(Db, CompilationDigest)) {
    TransactionType &Transaction = getRepoTransaction();
    {
      llvm::mc::repo::recordTicketDirectory(
          Transaction,
          pstore::index::get_index<pstore::trailer::indices::path>(Db),
          *OutputDir);

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

      // Find the store address of the target triple.
      auto const TriplePos = Names.find(stringRefAsView(TripleStr));
      assert(TriplePos != Names.end() && "Triple can't be found!");
      auto TripleAddr = TriplePos->second;

      // Set the definition's fragment extent.
      auto setFragmentExtent = [&RepoFragments, &FragmentsIndex,
                                &Db](pstore::repo::definition &Definition)
          -> llvm::Optional<pstore::index::fragment_index::const_iterator> {
        if (RepoFragments.find(Definition.digest) != RepoFragments.end()) {
          Definition.fext = RepoFragments[Definition.digest];
          return llvm::None;
        }
        auto It = FragmentsIndex->find(Db, Definition.digest);
        if (It != FragmentsIndex->end(Db)) {
          Definition.fext = It->second;
        }
        return It;
      };

      // The name field of each of definition is pointing into the 'Names'
      // map. Here we turn that into the pstore address of the string.
      for (pstore::repo::definition &Definition : CompilationDefinitions) {
        auto FragmentIndexIt = setFragmentExtent(Definition);

#ifndef NDEBUG
        // Check that we have a fragment for this ticket member's digest
        // value.
        // TODO: remove this check once we're completely confident in the
        // back-end implementation.
        if (!FragmentIndexIt) {
          FragmentIndexIt = FragmentsIndex->find(Db, Definition.digest);
        }
#endif // NDEBUG

        if (FragmentIndexIt &&
            FragmentIndexIt.getValue() == FragmentsIndex->end(Db)) {
          report_fatal_error("The digest of missing repository fragment " +
                             Definition.digest.to_hex_string() +
                             " was found in a definition.");
        }

        auto MNC = reinterpret_cast<ModuleNamesContainer::value_type const *>(
            Definition.name.absolute());
        Definition.name = MNC->second;
        LLVM_DEBUG(dbgs() << " definition name '" << stringViewAsRef(MNC->first)
                          << "' digest '" << Definition.digest << "' adding."
                          << '\n');
      }

      // Store the Compilation.
      auto CExtent = pstore::repo::compilation::alloc(
          Transaction, TripleAddr, CompilationDefinitions.begin(),
          CompilationDefinitions.end());
      CompilationIndex->insert(Transaction,
                               std::make_pair(CompilationDigest, CExtent));

      // Update the linked-definitions for each fragment index->address
      for (auto &KV : RepoFragments) {
        std::shared_ptr<pstore::repo::fragment> Fragment =
            pstore::repo::fragment::load(Transaction, KV.second);
        if (auto *const LinkedDefinitions =
                Fragment
                    ->atp<pstore::repo::section_kind::linked_definitions>()) {
          updateLinkedDefinitions(*LinkedDefinitions,
                                  *pstore::repo::compilation::load(Db, CExtent),
                                  CExtent.addr);
        }
      }
    }
    Transaction.commit();
  }

  // write the ticket file itself
  llvm::mc::repo::writeTicketFile(W, Db, CompilationDigest);

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
