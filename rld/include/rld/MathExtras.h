#ifndef RLD_MATH_EXTRAS_H
#define RLD_MATH_EXTRAS_H

#include "llvm/Support/MathExtras.h"
#include <cassert>

namespace rld {

inline uint64_t alignTo(uint64_t Value, std::size_t Align) noexcept {
  assert(llvm::countPopulation(Align) == 1U &&
         "Alignment must be a power of 2");
  return (Value + Align - 1U) & ~(Align - 1U);
  ;
}

} // end namespace rld

#endif // RLD_MATH_EXTRAS_H
