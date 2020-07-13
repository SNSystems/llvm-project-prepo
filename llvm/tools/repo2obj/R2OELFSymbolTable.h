//===-- R2OELFSymbolTable.h -----------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_TOOLS_REPO2OBJ_ELFSYMBOLTABLE_H
#define LLVM_TOOLS_REPO2OBJ_ELFSYMBOLTABLE_H

#include "llvm/Object/ELF.h"

#include "R2OELFSectionType.h"
#include "R2OELFStringTable.h"
#include "WriteHelpers.h"

#include <unordered_map>

template <typename ELFT> class OutputSection;

template <typename ELFT> class SymbolTable {
public:
  explicit SymbolTable(StringTable &Strings) : Strings_{Strings} {}
  SymbolTable(SymbolTable const &) = delete;
  SymbolTable &operator=(SymbolTable const &) = delete;

  struct SymbolTarget {
    /// \param Section_  The ELF output section in which the symbol's data
    /// resides.
    /// \param Offset_  The offset within Section which contains the first byte
    /// of the object.
    /// \param Size_  The object's size (in bytes).
    /// \param Linkage_  The symbol's linkage.
    /// \param Alignment_  The alignment of the symbol's target data.
    /// \param Visibility_  The symbol's ELF visibility.
    SymbolTarget(OutputSection<ELFT> const *Section_, std::uint64_t Offset_,
                 std::uint64_t Size_, pstore::repo::linkage Linkage_,
                 unsigned Alignment_, pstore::repo::visibility Visibility_)
        : Section{Section_}, Offset{Offset_}, Size{Size_}, Linkage{Linkage_},
          Alignment{Alignment_}, Visibility{Visibility_} {
      assert(Linkage_ == pstore::repo::linkage::common || Section != nullptr);
    }

    OutputSection<ELFT> const *Section;
    std::uint64_t Offset;
    std::uint64_t Size;
    pstore::repo::linkage Linkage;
    unsigned Alignment;
    pstore::repo::visibility Visibility;
  };

  struct Value {
    Value() = default;
    Value(std::uint64_t NameOffset_, llvm::Optional<SymbolTarget> Target_)
        : NameOffset{NameOffset_}, Target{std::move(Target_)} {}

    pstore::repo::linkage linkage() const {
      return Target ? Target.getValue().Linkage
                    : pstore::repo::linkage::external;
    }

    /// The offset of the name of this symbol in the symbol names string table.
    std::uint64_t NameOffset = 0;
    /// The location addressed by this symbol (if known).
    llvm::Optional<SymbolTarget> Target;
    /// True if this symbol type is ELF::STT_TLS, otherwise is false.
    bool IsTLS = false; // FIXME: two bools are unnessary because a symbol
                        // cannot be both TLS and common.
    bool IsCommon = false;

    std::uint64_t Index = llvm::ELF::STN_UNDEF;
  };

  /// Find a symbol with name equivalent to \p Name in the symbol table.
  /// \param Name  The symbol name.
  /// \returns A pointer to pre-existing entry for this name in the symbol
  /// table. If not found, return nullptr.
  Value *findSymbol(pstore::indirect_string const &Name);

  /// Creates a definition in the symbol table.
  /// \param Name  The symbol name.
  /// \param Section  The ELF output section in which the symbol's data resides.
  /// \param Offset  The offset within Section which contains the first byte of
  /// the object.
  /// \param Size  The object's size (in bytes).
  /// \param Linkage  The symbol's linkage.
  /// \param Alignment  The alignment of the symbol's target data.
  /// \param Visibility  The symbol's ELF visibility.
  /// \returns A pointer to the newly created or pre-existing entry for this
  /// name in the symbol table.
  Value *insertSymbol(pstore::indirect_string const &Name,
                      OutputSection<ELFT> const *Section, std::uint64_t Offset,
                      std::uint64_t Size, pstore::repo::linkage Linkage,
                      unsigned Alignment, pstore::repo::visibility Visibility);

  /// If not already in the symbol table, an undef entry is created. This may be
  /// later turned into a proper definition by a subsequent call to insertSymbol
  /// with the same name.
  ///
  /// The symbol relocation type is used to correctly tag the ELF symbol as TLS
  /// if necessary.
  ///
  /// \param Name  The symbol name.
  /// \param Type  The symbol relocation type.
  /// \returns A pointer to the newly created or pre-existing entry for this
  /// name in the symbol table.
  Value *insertSymbol(pstore::indirect_string const &Name,
                      pstore::repo::relocation_type Type);

  /// Writes the symbol table.
  /// \param OS The stream to which the symbol table is to be written.
  /// \param OrderedSymbols  The symbols to be written, ordered such that
  /// symbols with STB_LOCAL binding precede the weak and global symbols.
  /// \returns A tuple of three values, the first of which is the file offset at
  /// which the section data was written; the second is the number of bytes that
  /// were written; the third is true if we must also write an SHT_SYMTAB_SHNDX
  /// section.
  std::tuple<std::uint64_t, std::uint64_t, bool>
  write(llvm::raw_ostream &OS, std::vector<Value *> const &OrderedSymbols);

  /// Writes the contents of an SHT_SYMTAB_SHNDX section.
  /// \param OS The stream to which the extended section indices table is to be
  /// written.
  /// \param OrderedSymbols  The symbols to be written, ordered such that
  /// symbols with STB_LOCAL binding precede the weak and global symbols.
  /// \returns A tuple of two values: the first is the file offset of the start
  /// of the table; the second is the number of bytes that were written.
  std::tuple<std::uint64_t, std::uint64_t>
  writeSymtabShndx(llvm::raw_ostream &OS,
                   std::vector<Value *> const &OrderedSymbols);

  /// As a side effect, sets the Index field on the symbol entries to allow the
  /// index of any symbol to be quickly discovered.
  /// \note Don't insert any symbols after calling this function!
  std::vector<Value *> sort();

  static unsigned firstNonLocal(std::vector<Value *> const &OrderedSymbols);

private:
  static unsigned linkageToELFBinding(pstore::repo::linkage L);
  static unsigned char visibilityToELFOther(pstore::repo::visibility SV);
  static unsigned sectionToSymbolType(ELFSectionType T);
  /// Returns the ELF symbol type for a symbol value.
  static unsigned valueToSymbolType(const Value &SV);

  static bool isTLSRelocation(pstore::repo::relocation_type Type);

  using Elf_Sym = typename llvm::object::ELFFile<ELFT>::Elf_Sym;
  using Elf_Half = typename ELFT::Half;
  using Elf_Word = typename ELFT::Word;

  /// Given a symbol value, returns the value for the symbols st_shndx.
  static Elf_Half getTargetShndx(Value const &SV);

  Value *insertSymbol(pstore::indirect_string const &Name,
                      llvm::Optional<SymbolTarget> const &Target);

  std::unordered_map<pstore::indirect_string, Value> SymbolMap_;
  StringTable &Strings_;
};

template <typename ELFT>
unsigned SymbolTable<ELFT>::linkageToELFBinding(pstore::repo::linkage L) {
  switch (L) {
  case pstore::repo::linkage::internal_no_symbol:
  case pstore::repo::linkage::internal:
    return llvm::ELF::STB_LOCAL;
  case pstore::repo::linkage::link_once_any:
  case pstore::repo::linkage::link_once_odr:
  case pstore::repo::linkage::weak_any:
  case pstore::repo::linkage::weak_odr:
    return llvm::ELF::STB_WEAK;
  default:
    return llvm::ELF::STB_GLOBAL;
  }
}

template <typename ELFT>
unsigned char
SymbolTable<ELFT>::visibilityToELFOther(pstore::repo::visibility SV) {
  switch (SV) {
  case pstore::repo::visibility::default_vis:
    return llvm::ELF::STV_DEFAULT;
  case pstore::repo::visibility::hidden_vis:
    return llvm::ELF::STV_HIDDEN;
  case pstore::repo::visibility::protected_vis:
    return llvm::ELF::STV_PROTECTED;
  }
  llvm_unreachable("Unsupported visibility type");
}

template <typename ELFT>
unsigned SymbolTable<ELFT>::sectionToSymbolType(ELFSectionType T) {
  using namespace pstore::repo;
  switch (T) {
  case ELFSectionType::text:
    return llvm::ELF::STT_FUNC;
  case ELFSectionType::bss:
  case ELFSectionType::data:
  case ELFSectionType::init_array:
  case ELFSectionType::fini_array:
  case ELFSectionType::rel_ro:
  case ELFSectionType::mergeable_1_byte_c_string:
  case ELFSectionType::mergeable_2_byte_c_string:
  case ELFSectionType::mergeable_4_byte_c_string:
  case ELFSectionType::mergeable_const_4:
  case ELFSectionType::mergeable_const_8:
  case ELFSectionType::mergeable_const_16:
  case ELFSectionType::mergeable_const_32:
  case ELFSectionType::read_only:
    return llvm::ELF::STT_OBJECT;
  case ELFSectionType::thread_bss:
  case ELFSectionType::thread_data:
    return llvm::ELF::STT_TLS;
  case ELFSectionType::debug_line:
  case ELFSectionType::debug_ranges:
  case ELFSectionType::debug_string:
  case ELFSectionType::interp:
    return llvm::ELF::STT_NOTYPE;
  }
  llvm_unreachable("invalid section type");
}

template <typename ELFT>
unsigned SymbolTable<ELFT>::valueToSymbolType(const Value &SV) {
  if (SV.Target) {
    OutputSection<ELFT> const *const Section = SV.Target->Section;
    const auto ST = Section != nullptr
                        ? sectionToSymbolType(Section->getType())
                        : static_cast<unsigned>(llvm::ELF::STT_OBJECT);
    assert(SV.IsTLS == (ST == llvm::ELF::STT_TLS));
    return ST;
  }

  if (SV.IsTLS) {
    return llvm::ELF::STT_TLS;
  }
  if (SV.IsCommon) {
    return llvm::ELF::STT_COMMON;
  }
  return llvm::ELF::STT_NOTYPE;
}

template <typename ELFT>
bool SymbolTable<ELFT>::isTLSRelocation(pstore::repo::relocation_type Type) {
  switch (Type) {
  case llvm::ELF::R_X86_64_DTPMOD64:
  case llvm::ELF::R_X86_64_DTPOFF64:
  case llvm::ELF::R_X86_64_TPOFF64:
  case llvm::ELF::R_X86_64_TLSGD:
  case llvm::ELF::R_X86_64_TLSLD:
  case llvm::ELF::R_X86_64_DTPOFF32:
  case llvm::ELF::R_X86_64_GOTTPOFF:
  case llvm::ELF::R_X86_64_TPOFF32:
    return true;
  default:
    return false;
  }
}

// findSymbol
// ~~~~~~~~~~
template <typename ELFT>
auto SymbolTable<ELFT>::findSymbol(pstore::indirect_string const &Name)
    -> Value * {
  auto Pos = SymbolMap_.find(Name);
  return Pos != SymbolMap_.end() ? &Pos->second : nullptr;
}

// insertSymbol
// ~~~~~~~~~~~~
template <typename ELFT>
auto SymbolTable<ELFT>::insertSymbol(pstore::indirect_string const &Name,
                                     OutputSection<ELFT> const *Section,
                                     std::uint64_t Offset, std::uint64_t Size,
                                     pstore::repo::linkage Linkage,
                                     unsigned Alignment,
                                     pstore::repo::visibility Visibility)
    -> Value * {
  auto SV =
      this->insertSymbol(Name, SymbolTarget(Section, Offset, Size, Linkage,
                                            Alignment, Visibility));
  if (Linkage == pstore::repo::linkage::common) {
    SV->IsCommon = true;
  } else {
    SV->IsTLS = sectionToSymbolType(Section->getType()) == llvm::ELF::STT_TLS;
  }
  return SV;
}

template <typename ELFT>
auto SymbolTable<ELFT>::insertSymbol(pstore::indirect_string const &Name,
                                     pstore::repo::relocation_type Type)
    -> Value * {
  auto SV = this->insertSymbol(Name, llvm::None);
  SV->IsTLS = isTLSRelocation(Type);
  return SV;
}

template <typename ELFT>
auto SymbolTable<ELFT>::insertSymbol(pstore::indirect_string const &Name,
                                     llvm::Optional<SymbolTarget> const &Target)
    -> Value * {
  typename decltype(SymbolMap_)::iterator Pos;
  bool DidInsert;
  std::tie(Pos, DidInsert) = SymbolMap_.emplace(Name, Value{});
  if (DidInsert) {
    Pos->second = Value{Strings_.insert(Name), Target};
    return &Pos->second;
  }

  // If we don't have a value associated with this symbol, then use the one we
  // have here.
  Value &V = Pos->second;
  if (!V.Target) {
    // FIXME: if Target && V.Target, we're attempting to make a duplicate
    // definition which wouldn't be right.
    V.Target = Target;
  }
  return &Pos->second;
}

// getTargetShndx
// ~~~~~~~~~~~~~~
template <typename ELFT>
auto SymbolTable<ELFT>::getTargetShndx(Value const &SV) -> Elf_Half {
  if (SV.IsCommon) {
    return Elf_Half{llvm::ELF::SHN_COMMON};
  }
  if (!SV.Target) {
    return Elf_Half{llvm::ELF::SHN_UNDEF};
  }
  size_t const TargetSectionIndex = SV.Target->Section->getIndex();
  return TargetSectionIndex < llvm::ELF::SHN_LORESERVE
             ? static_cast<Elf_Half>(TargetSectionIndex)
             : Elf_Half{llvm::ELF::SHN_XINDEX};
}

// write
// ~~~~~
template <typename ELFT>
std::tuple<std::uint64_t, std::uint64_t, bool>
SymbolTable<ELFT>::write(llvm::raw_ostream &OS,
                         std::vector<Value *> const &OrderedSymbols) {
  using namespace llvm;
  writeAlignmentPadding<Elf_Sym>(OS);

  uint64_t const StartOffset = OS.tell();
  bool NeedsExtendedIndices = false;

  // Write the reserved zeroth symbol table entry.
  Elf_Sym Symbol;
  zero(Symbol);
  Symbol.st_shndx = ELF::SHN_UNDEF;
  writeRaw(OS, Symbol);

  for (const Value *const SV : OrderedSymbols) {
    zero(Symbol);
    Symbol.st_name = SV->NameOffset;

    if (SV->Target) {
      SymbolTarget const &T = SV->Target.getValue();
      Symbol.st_value = SV->IsCommon ? T.Alignment : T.Offset;
      Symbol.setBindingAndType(linkageToELFBinding(T.Linkage),
                               valueToSymbolType(*SV));
      Symbol.st_other = visibilityToELFOther(T.Visibility);
      // The section (header table index) in which this value is defined.
      Symbol.st_shndx = getTargetShndx(*SV);
      Symbol.st_size = T.Size;
      if (Symbol.st_shndx == ELF::SHN_XINDEX) {
        NeedsExtendedIndices = true;
      }
    } else {
      // There's no definition for this name.
      Symbol.setBindingAndType(ELF::STB_GLOBAL, valueToSymbolType(*SV));
      Symbol.st_shndx = getTargetShndx(*SV);
    }

    writeRaw(OS, Symbol);
  }
  return std::make_tuple(StartOffset, OS.tell() - StartOffset,
                         NeedsExtendedIndices);
}

// writeSymtabShndx
// ~~~~~~~~~~~~~~~~
// Writes the contents of an SHT_SYMTAB_SHNDX section. It contains an array of
// Elf32_Word values. This array contains one entry for every entry in the
// associated symbol table entry. The values represent the section header
// indexes against which the symbol table entries are defined. Only if
// corresponding symbol table entry's st_shndx field contains the escape value
// SHN_XINDEX will the matching Elf32_Word hold the actual section header index.
// Otherwise, the entry must be SHN_UNDEF (0).
template <typename ELFT>
std::tuple<std::uint64_t, std::uint64_t> SymbolTable<ELFT>::writeSymtabShndx(
    llvm::raw_ostream &OS, std::vector<Value *> const &OrderedSymbols) {
  writeAlignmentPadding<Elf_Word>(OS);

  uint64_t const StartOffset = OS.tell();
  // The reserved zeroth entry.
  std::uint64_t Size = writeRaw(OS, Elf_Word{llvm::ELF::SHN_UNDEF});

  for (const Value *const SV : OrderedSymbols) {
    const Elf_Half shndx = SymbolTable<ELFT>::getTargetShndx(*SV);
    Size += writeRaw(
        OS, shndx == llvm::ELF::SHN_XINDEX
                ? static_cast<Elf_Word>(SV->Target->Section->getIndex())
                : Elf_Word{llvm::ELF::SHN_UNDEF});
  }
  assert(Size == OS.tell() - StartOffset);
  return std::make_tuple(StartOffset, Size);
}

template <typename ELFT>
auto SymbolTable<ELFT>::sort() -> std::vector<Value *> {
  std::vector<Value *> OrderedSymbols;
  OrderedSymbols.reserve(SymbolMap_.size());
  for (auto &S : SymbolMap_) {
    OrderedSymbols.push_back(&S.second);
  }

  // Use stable_sort() to preserve the alphabetical ordering of the two groups
  // (internal and non-internal) of symbols. This makes the repo2obj symbol
  // table a little more similar to the compiler's own ELF files.
  std::stable_sort(
      std::begin(OrderedSymbols), std::end(OrderedSymbols),
      [](Value const *const A, Value const *const B) {
        const auto ALinkage = A->linkage();
        const auto BLinkage = B->linkage();
        return (ALinkage == pstore::repo::linkage::internal ||
                ALinkage == pstore::repo::linkage::internal_no_symbol) &&
               (BLinkage != pstore::repo::linkage::internal &&
                BLinkage != pstore::repo::linkage::internal_no_symbol);
      });

  // Finally tell the symbols about their indices.
  unsigned Index = 0;
  for (auto &S : OrderedSymbols) {
    S->Index = ++Index;
  }

  return OrderedSymbols;
}

template <typename ELFT>
unsigned
SymbolTable<ELFT>::firstNonLocal(std::vector<Value *> const &OrderedSymbols) {
  using Iterator = typename std::vector<Value *>::const_iterator;
  auto Count =
      static_cast<typename std::iterator_traits<Iterator>::difference_type>(
          OrderedSymbols.size());
  auto First = std::begin(OrderedSymbols);

  while (Count > 0U) {
    auto It = First;
    auto const Step = Count / 2;
    std::advance(It, Step);

    const auto Linkage = (*It)->linkage();
    if (Linkage == pstore::repo::linkage::internal ||
        Linkage == pstore::repo::linkage::internal_no_symbol) {
      First = ++It;
      Count -= Step + 1;
    } else {
      Count = Step;
    }
  }
  return First != std::end(OrderedSymbols) ? (*First)->Index : 1U;
}

#endif // LLVM_TOOLS_REPO2OBJ_ELFSYMBOLTABLE_H
