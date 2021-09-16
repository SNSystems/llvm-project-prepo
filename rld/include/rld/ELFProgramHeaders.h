#ifndef RLD_ELFPROGRAMHEADERS_H
#define RLD_ELFPROGRAMHEADERS_H

#include "rld/LayoutBuilder.h"

#include "llvm/Object/ELF.h"

namespace rld {
namespace elf {

template <typename ELFT>
constexpr auto elfSegmentFlags(const SegmentKind Kind) ->
    typename llvm::object::ELFFile<ELFT>::Elf_Word {
  using Elf_Word = typename llvm::object::ELFFile<ELFT>::Elf_Word;
  switch (Kind) {
  case SegmentKind::data:
  case SegmentKind::gnu_relro:
  case SegmentKind::gnu_stack:
    return Elf_Word{llvm::ELF::PF_R | llvm::ELF::PF_W};

  case SegmentKind::phdr:
  case SegmentKind::rodata:
  case SegmentKind::tls:
    return Elf_Word{llvm::ELF::PF_R};

  case SegmentKind::text:
    return Elf_Word{llvm::ELF::PF_X | llvm::ELF::PF_R};

  case SegmentKind::discard:
  case SegmentKind::last:
    break;
  }
  llvm_unreachable("Invalid segment kind");
}

template <typename ELFT>
typename llvm::object::ELFFile<ELFT>::Elf_Phdr *emitProgramHeaders(
    typename llvm::object::ELFFile<ELFT>::Elf_Phdr *Phdr, Context &Ctxt,
    const FileRegion &TargetDataRegion, const FileRegion &SegmentTableRegion,
    const Layout &Lout,
    const SegmentIndexedArray<llvm::Optional<int64_t>> &SegmentDataOffsets);

} // end namespace elf
} // end namespace rld

#endif // RLD_ELFPROGRAMHEADERS_H
