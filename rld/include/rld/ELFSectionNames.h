#ifndef RLD_ELFSECTIONNAMES_H
#define RLD_ELFSECTIONNAMES_H

#include "rld/LayoutBuilder.h"

namespace rld {
namespace elf {

SectionIndexedArray<uint64_t> buildSectionNameStringTable(Layout *const Lout);

uint8_t *sectionNameTableWriter(uint8_t *Data, const Layout &Lout);

} // end namespace elf
} // end namespace rld

#endif // RLD_ELFSECTIONNAMES_H
