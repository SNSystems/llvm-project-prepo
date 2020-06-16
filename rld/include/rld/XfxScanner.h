#ifndef RLD_XFX_SCANNER_H
#define RLD_XFX_SCANNER_H

#include "rld/LayoutBuilder.h"
#include "rld/symbol.h"

namespace rld {

bool resolveXfixups(Context &Context, LocalSymbolsContainer const &Locals,
                    NotNull<rld::GlobalSymbolsContainer *> const Globals,
                    NotNull<UndefsContainer *> const Undefs,
                    uint32_t InputOrdinal);

} // end namespace rld

#endif // RLD_XFX_SCANNER_H
