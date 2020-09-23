#include "rld/copy.h"

#include "rld/MathExtras.h"

#include "llvm/Support/ThreadPool.h"

#include <cassert>

using namespace rld;

namespace {

constexpr auto DebugType = "rld-copy";

} // end anonymous namespace

template <pstore::repo::section_kind SKind,
          typename SType = typename pstore::repo::enum_to_section<SKind>::type>
static void copySection(Context &Ctxt, Contribution const &Contribution,
                        std::uint8_t *Dest) {
  auto *const Section = reinterpret_cast<SType const *>(Contribution.Section);

  llvmDebug(DebugType, Ctxt.IOMut, [Dest]() {
    llvm::dbgs() << "copy to "
                 << format_hex(reinterpret_cast<std::uintptr_t>(Dest)) << '\n';
  });

  auto const d = Section->payload();
  std::memcpy(Dest, d.begin(), d.size());

  // auto * const shadow = Ctxt.shadow();

#if 0
  for (auto const &XFixup : Section->xfixups()) {

      switch (XFixup.type) {
      // target-dependent relocations here...
            typed_address<indirect_string> name;
            relocation_type type;
            std::uint8_t padding1 = 0;
            std::uint16_t padding2 = 0;
            std::uint32_t padding3 = 0;
            std::uint64_t offset;
            std::uint64_t addend;
    }
  }
#endif
}

template <>
void copySection<pstore::repo::section_kind::bss>(Context &Ctxt,
                                                  Contribution const &S,
                                                  std::uint8_t *Dest) {
  llvmDebug(DebugType, Ctxt.IOMut, [Dest]() {
    llvm::dbgs() << "BSS fill " << reinterpret_cast<std::uintptr_t>(Dest)
                 << '\n';
  });
  static_assert(
      std::is_same<
          pstore::repo::enum_to_section<pstore::repo::section_kind::bss>::type,
          pstore::repo::bss_section>::value,
      "BSS section kind must map to BSS section type");
}

template <>
void copySection<pstore::repo::section_kind::linked_definitions>(
    Context &, Contribution const &, std::uint8_t *) {
  // discard
}

namespace rld {

void copyToOutput(
    Context &Ctxt, llvm::ThreadPool &Workers, uint8_t *const Data,
    const Layout &L,
    rld::SectionArray<llvm::Optional<uint64_t>> &SectionFileOffsets,
    uint64_t TargetDataOffset) {

  for (SectionKind SectionK = firstSectionKind(); SectionK != SectionKind::last;
       ++SectionK) {
    const OutputSection::ContributionVector &Contributions =
        L.Sections[SectionK].Contributions;
    if (Contributions.empty()) {
      continue;
    }
    assert(SectionFileOffsets[SectionK].hasValue() &&
           "No layout position for a section with contributions");
    Workers.async(
        [SectionK, &Ctxt, Data, &Contributions](uint64_t Start) {
          for (Contribution const &Contribution : Contributions) {
            llvmDebug(DebugType, Ctxt.IOMut,
                      [SectionK]() { llvm::dbgs() << SectionK << ": "; });

            auto *const Dest =
                Data + alignTo(Contribution.Offset, Contribution.Align) + Start;
            // FIXME: not all values of rld::SectionKind can be cast to
            // repo::section_kind!
            switch (static_cast<pstore::repo::section_kind>(SectionK)) {
#define X(a)                                                                   \
  case pstore::repo::section_kind::a:                                          \
    copySection<pstore::repo::section_kind::a>(Ctxt, Contribution, Dest);      \
    break;

              PSTORE_MCREPO_SECTION_KINDS
#undef X
            default:
              llvm_unreachable("Bad section kind");
              break;
            }
          }
        },
        TargetDataOffset + *SectionFileOffsets[SectionK]);
  }
}

} // end namespace rld
