#include "rld/Shadow.h"

#include "rld/symbol.h"

static_assert(
    alignof(rld::Symbol) > 1U,
    "The LSB of pointers is used to distinguish between CompilationRef* "
    "and Symbol*");
static_assert(
    alignof(rld::CompilationRef) > 1U,
    "The LSB of pointers is used to distinguish between CompilationRef* "
    "and Symbol*");

const rld::shadow::TaggedPointer rld::shadow::TaggedPointer::Busy{
    reinterpret_cast<void *>(std::numeric_limits<uintptr_t>::max())};
