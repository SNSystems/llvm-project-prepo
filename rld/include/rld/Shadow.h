#ifndef RLD_SHADOW_H
#define RLD_SHADOW_H

#include "rld/symbol.h"

#include <atomic>
#include <cassert>

namespace rld {

class Symbol;
struct ArchDef;

namespace shadow {

// Shadow pointer state transitions:
//
//            +------+
//            | null |
//            +------+
//               |
//               v
//            +------+
// +--------->| busy |<----------+
// |          +------+           |
// |(1)          |            (2)|
// |       +-----+------+        |
// |       v            v        |
// |  +---------+  +----------+  |
// +--| symbol* |  | archdef* |--+
//    +---------+  +----------+
//
// Notes:
// (1) State changes from an undef symbol to busy and back to the same undef
// symbol. (2) We can go from an archdef to a defined symbol.

static_assert(
    alignof(Symbol) > 1U,
    "The LSB of pointers is used to distinguish between ArchDef* and Symbol*");
static_assert(
    alignof(Symbol) > 1U,
    "The LSB of pointers is used to distinguish between ArchDef* and Symbol*");
constexpr auto archdef_mask = std::uintptr_t{0x01};

inline ArchDef *asArchDef(void const *P) {
  auto const uintptr = reinterpret_cast<std::uintptr_t>(P);
  if (uintptr & archdef_mask) {
    return reinterpret_cast<ArchDef *>(uintptr & ~archdef_mask);
  }
  return nullptr;
}

inline void *tagged(Symbol *const sym) {
  assert(asArchDef(sym) == nullptr);
  return sym;
}
inline void *tagged(ArchDef *const ad) {
  assert(asArchDef(ad) == nullptr);
  return reinterpret_cast<void *>(reinterpret_cast<std::uintptr_t>(ad) |
                                  archdef_mask);
}

using void_ptr = void *;
using atomic_void_ptr = std::atomic<void *>;

auto *const busy =
    reinterpret_cast<void_ptr>(std::numeric_limits<uintptr_t>::max());

namespace details {

/// Performs a nullptr -> busy -> symbol*/archdef* state transition.
///
/// \tparam Create  A function with signature symbol*() or archdef*().
/// \param p  A pointer to the atomic to be set. This should lie within the
/// repository
///   shadow memory area.
/// \param expected  On return, the value contained by the atomic.
/// \param create  A function called to create a new symbol or archdef.
template <typename Create>
inline bool null_to_final(atomic_void_ptr *const p, void_ptr &expected,
                          Create create) {
  expected = nullptr;
  if (p->compare_exchange_strong(expected, busy, std::memory_order_acq_rel,
                                 std::memory_order_relaxed)) {
    expected = tagged(create());
    p->store(expected, std::memory_order_release);
    return true;
  }
  return false;
}

/// Performs a archdef* -> busy -> symbol*/archdef* state transition.
///
/// \tparam CreateFromArchdef  A function with signature symbol*(archdef *) or
///    archdef*(archdef *).
/// \param p  A pointer to the atomic to be set. This should lie within
///   the repository shadow memory area.
/// \param expected  On entry, must point to an archdef pointer.
/// \param create_from_archdef  A function called to update an archdef or to
/// create a symbol
///   based on the input archdef.
///
/// \note This function expected to be called from within a loop which checks
/// the value of
///   expected. This enables use of compare_exchange_weak() which may be
///   slightly faster than the _strong() function on some platforms.
template <typename CreateFromArchdef>
inline bool archdef_to_final(atomic_void_ptr *const p, void_ptr &expected,
                             CreateFromArchdef create_from_archdef) {
  ArchDef *const AD = asArchDef(expected);
  assert(AD != nullptr);
  if (p->compare_exchange_weak(expected, busy, std::memory_order_acq_rel,
                               std::memory_order_relaxed)) {
    expected = tagged(create_from_archdef(AD));
    p->store(expected, std::memory_order_release);
    return true;
  }
  return false;
}

/// Performs a symbol* -> busy -> symbol* state transition.
///
/// \tparam Update  A function with signature symbol*(symbol*).
/// \param p  A pointer to the atomic to be set. This should lie within the
/// repository
///   shadow memory area.
/// \param expected  On entry, must point to a symbol pointer.
/// \param update  A function used to update the symbol to which \p expected
/// points. This
///   function may adjust the body of the symbol or point it to a different
///   symbol instance altogether.
/// \returns True if the operation was performed, false if retry is necessary.
///
/// \note This function expected to be called from within a loop which checks
/// the value of
///   expected. This enables use of compare_exchange_weak() which may be
///   slightly faster than the _strong() function on some platforms.
template <typename Update>
inline bool symbol_to_final(atomic_void_ptr *const p, void_ptr &expected,
                            Update const update) {
  assert(asArchDef(expected) == nullptr);
  auto *const Sym = reinterpret_cast<Symbol *>(expected);
  if (p->compare_exchange_weak(expected, busy, std::memory_order_acq_rel,
                               std::memory_order_relaxed)) {
    Symbol *const Sym2 = update(Sym);
    expected = tagged(Sym2);
    p->store(expected, std::memory_order_release);
    return true;
  }
  return false;
}

/// \param p  A pointer to the atomic to be set. This should lie within the
/// repository
///   shadow memory area.
/// \returns The value contained within the atomic.
inline void *spin_while_busy(atomic_void_ptr *const p) {
  void *expected = busy;
  while ((expected = p->load(std::memory_order_acquire)) == busy) {
    std::this_thread::yield();
  }
  return expected;
}

} // end namespace details

/// \tparam Create  A function with signature symbol*() or archdef*().
/// \tparam CreateFromArchdef  A function with signature symbol*(archdef *) or
///   archdef*(archdef*).
/// \tparam Update A function with signature symbol*(symbol*).
///
/// \param p  A pointer to the atomic to be set. This should lie within the
/// repository shadow
///   memory area.
/// \param create  A function called to create a new symbol or archdef.
/// \param create_from_archdef  A function called to update an archdef or to
/// create a symbol
///   based on the input archdef.
/// \param update  A function used to update the symbol to which \p expected
/// points. This
///   function may adjust the body of the symbol or point it to a different
///   symbol instance altogether.
template <typename Create, typename CreateFromArchdef, typename Update>
void set(atomic_void_ptr *const p, Create const create,
         CreateFromArchdef const create_from_archdef, Update const update) {
  // null -> busy -> symbol*/archdef*
  void *expected = nullptr;
  if (details::null_to_final(p, expected, create)) {
    return;
  }
  for (;;) {
    if (expected == busy) {
      expected = details::spin_while_busy(p);
    }
    if (ArchDef *const ad = asArchDef(expected)) {
      // archdef* -> busy -> symbol*/archdef*
      if (details::archdef_to_final(p, expected, create_from_archdef)) {
        return;
      }
    } else {
      // symbol* -> busy -> symbol*
      if (details::symbol_to_final(p, expected, update)) {
        return;
      }
    }
  }
}

/// \tparam Create  A function with signature symbol*() or archdef*().
/// \tparam CreateFromArchdef  A function with signature symbol*(archdef *) or
///   archdef*(archdef*).
///
/// \param p  A pointer to the atomic to be set. This should lie within the
/// repository shadow
///   memory area.
/// \param create  A function called to create a new symbol or archdef.
/// \param create_from_archdef  A function called to update an archdef or to
/// create a symbol
///   based on the input archdef.
template <typename Create, typename CreateFromArchdef>
void set(atomic_void_ptr *const p, Create const create,
         CreateFromArchdef const create_from_archdef) {
  // null -> busy -> symbol*/archdef*
  void *expected = nullptr;
  if (details::null_to_final(p, expected, create)) {
    return;
  }
  for (;;) {
    if (expected == busy) {
      expected = details::spin_while_busy(p);
    }
    if (ArchDef *const ad = asArchDef(expected)) {
      // archdef* -> busy -> symbol*/archdef*
      if (details::archdef_to_final(p, expected, create_from_archdef)) {
        return;
      }
    } else {
      // This is already a symbol*.
      return;
    }
  }
}

} // end namespace shadow
} // end namespace rld

#endif // RLD_SHADOW_HPP
