#ifndef RLD_ELFSECTIONHEADERS_H
#define RLD_ELFSECTIONHEADERS_H

#include "rld/SectionArray.h"

#include "llvm/Object/ELF.h"

namespace rld {

class Layout;

namespace elf {

template <typename ELFT>
typename llvm::object::ELFFile<ELFT>::Elf_Shdr *emitSectionHeaders(
    typename llvm::object::ELFFile<ELFT>::Elf_Shdr *Shdr, const Layout &Lout,
    const SectionArray<llvm::Optional<int64_t>> &SectionFileOffsets,
    const EnumIndexedArray<SectionKind, SectionKind::last, uint64_t>
        &NameOffsets,
    size_t LocalsSize, uint64_t TargetDataOffset);

} // end namespace elf
} // end namespace rld

#endif // RLD_ELFSECTIONHEADERS_H
