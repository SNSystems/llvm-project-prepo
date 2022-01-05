#ifndef RLD_SHADOW_H
#define RLD_SHADOW_H

#include <atomic>
#include <cassert>
#include <thread>
#include <type_traits>

namespace rld {

class Symbol;
struct CompilationRef;

namespace shadow {

// Shadow pointer state transitions:
//
//            +------+
//            | null |
//            +------+
//               |
//               v
//            +------+
// +--------->| Busy |<--------------+
// |          +------+               |
// |(1)          |                (2)|
// |       +-----+------+            |
// |       v            v            |
// |  +---------+  +-----------------+
// +--| Symbol* |  | CompilationRef* |
//    +---------+  +-----------------+
//
// Notes:
// (1) State changes from an undef symbol to busy and back to the same undef
// symbol. (2) We can go from an CompilationRef to a defined Symbol.

// **************
// Tagged Pointer
// **************
class TaggedPointer {
  friend class AtomicTaggedPointer;

public:
  static const TaggedPointer Busy;

  TaggedPointer() = default;
  explicit TaggedPointer(Symbol *const Sym);
  explicit TaggedPointer(CompilationRef *const CR);

  constexpr bool operator==(const TaggedPointer &Other) const {
    return Ptr_ == Other.Ptr_;
  }
  bool operator==(const Symbol *const Sym) const;
  bool operator==(const CompilationRef *const CR) const;

  constexpr bool operator!=(const TaggedPointer &Other) const {
    return !operator==(Other);
  }
  bool operator!=(const Symbol *const Other) const {
    return !operator==(Other);
  }
  bool operator!=(const CompilationRef *const Other) const {
    return !operator==(Other);
  }

  constexpr explicit operator bool() const noexcept { return Ptr_ != nullptr; }

  template <typename T> T get_if();
  template <typename T> T get_if() const;

private:
  explicit constexpr TaggedPointer(void *const P) : Ptr_{P} {}
  static constexpr auto CompilationRefMask_ = std::uintptr_t{0x01};

  template <typename T, typename Result = typename std::conditional_t<
                            std::is_const<T>::value, const Symbol *, Symbol *>>
  static Result asSymbol(T *const P) {
    return (reinterpret_cast<std::uintptr_t>(P) & CompilationRefMask_)
               ? nullptr
               : reinterpret_cast<Result>(P);
  }

  template <typename T, typename Result = typename std::conditional_t<
                            std::is_const<T>::value, const CompilationRef *,
                            CompilationRef *>>
  static Result asCompilationRef(T *const P) {
    auto const uintptr = reinterpret_cast<std::uintptr_t>(P);
    return (uintptr & CompilationRefMask_)
               ? reinterpret_cast<Result>(uintptr & ~CompilationRefMask_)
               : nullptr;
  }

  void *Ptr_ = nullptr;
};

// (ctor)
// ~~~~~~
inline TaggedPointer::TaggedPointer(Symbol *const Sym) : Ptr_{Sym} {
  assert(TaggedPointer::asCompilationRef(Sym) == nullptr &&
         "Do not pass an CompilationRef-tagged pointer to tagged()");
}
inline TaggedPointer::TaggedPointer(CompilationRef *const CR)
    : Ptr_{reinterpret_cast<void *>(reinterpret_cast<std::uintptr_t>(CR) |
                                    CompilationRefMask_)} {
  assert(TaggedPointer::asCompilationRef(CR) == nullptr &&
         "Do not pass an CompilationRef-tagged pointer to tagged()");
}

// get if
// ~~~~~~
template <> inline Symbol *TaggedPointer::get_if<Symbol *>() {
  return TaggedPointer::asSymbol(Ptr_);
}
template <> inline const Symbol *TaggedPointer::get_if<const Symbol *>() const {
  return TaggedPointer::asSymbol(Ptr_);
}
template <> inline CompilationRef *TaggedPointer::get_if<CompilationRef *>() {
  return TaggedPointer::asCompilationRef(Ptr_);
}
template <>
inline const CompilationRef *
TaggedPointer::get_if<const CompilationRef *>() const {
  return TaggedPointer::asCompilationRef(Ptr_);
}

// operator==
// ~~~~~~~~~~
inline bool TaggedPointer::operator==(const Symbol *const Sym) const {
  return this->get_if<const Symbol *>() == Sym;
}
inline bool TaggedPointer::operator==(const CompilationRef *const CR) const {
  return this->get_if<const CompilationRef *>() == CR;
}

// *********************
// Atomic Tagged Pointer
// *********************
class AtomicTaggedPointer {
public:
  AtomicTaggedPointer() = default;
  explicit AtomicTaggedPointer(TaggedPointer TP) : P_{TP.Ptr_} {}
  AtomicTaggedPointer(AtomicTaggedPointer const &Other) = delete;
  AtomicTaggedPointer(AtomicTaggedPointer &&Other) noexcept = delete;

  ~AtomicTaggedPointer() noexcept = default;

  AtomicTaggedPointer &operator=(AtomicTaggedPointer const &Other) = delete;
  AtomicTaggedPointer &operator=(AtomicTaggedPointer &&Other) noexcept = delete;

  TaggedPointer
  load(std::memory_order Order = std::memory_order_seq_cst) const noexcept {
    return TaggedPointer{P_.load(Order)};
  }

  void store(TaggedPointer Desired,
             std::memory_order Order = std::memory_order_seq_cst) noexcept {
    P_.store(Desired.Ptr_, Order);
  }

  bool compare_exchange_weak(TaggedPointer &Expected, TaggedPointer Desired,
                             std::memory_order Success,
                             std::memory_order Failure) noexcept {
    return P_.compare_exchange_weak(Expected.Ptr_, Desired.Ptr_, Success,
                                    Failure);
  }

  bool compare_exchange_strong(TaggedPointer &Expected, TaggedPointer Desired,
                               std::memory_order Success,
                               std::memory_order Failure) noexcept {
    return P_.compare_exchange_strong(Expected.Ptr_, Desired.Ptr_, Success,
                                      Failure);
  }

private:
  std::atomic<void *> P_{nullptr};
};

namespace details {

/// Performs a nullptr -> Busy -> Symbol*/CompilationRef* state transition.
/// Called the first time that we encounter a symbol name.
///
/// \tparam CreateFn  A function with signature TaggedPointer().
/// \param P  A pointer to the atomic to be set. This should lie within the
/// repository
///   shadow memory area.
/// \param [in,out] Expected  On return, the value contained by the atomic.
/// \param Create  A function called to create a new Symbol or CompilationRef.
/// \returns True if the state transition was completed, false otherwise.
template <typename CreateFn>
inline bool nullToFinal(AtomicTaggedPointer *const P, TaggedPointer &Expected,
                        const CreateFn Create) {
  Expected = TaggedPointer{};
  if (P->compare_exchange_strong(Expected, TaggedPointer::Busy,
                                 std::memory_order_acq_rel,
                                 std::memory_order_relaxed)) {
    Expected = Create();
    P->store(Expected, std::memory_order_release);
    return true;
  }
  return false;
}

/// Performs a CompilationRef* -> Busy -> Symbol*/CompilationRef* state
/// transition.
///
/// \tparam CreateFromCompilationRefFn  A function with signature
///   TaggedPointer(std::atomic<void*>*, CompilationRef *).
/// \param P  A pointer to the atomic to be set. This should lie within the
/// repository
///   shadow memory area.
/// \param [in,out] Expected  On entry, must point to an CompilationRef
///   pointer.
/// \param CreateFromCompilationRef  A function called to update a
/// CompilationRef or to
///   create a symbol based on the input CompilationRef.
/// \returns  True if the state transition was completed, false otherwise.
///
/// \note This function expected to be called from within a loop which checks
/// the value of
///   expected. This enables use of compare_exchange_weak() which may be
///   slightly faster than compare_exchange_strong() alternative on some
///   platforms.
template <typename CreateFromCompilationRefFn>
inline bool compilationRefToFinal(
    AtomicTaggedPointer *const P, TaggedPointer &Expected,
    const CreateFromCompilationRefFn CreateFromCompilationRef) {
  auto *const CR = Expected.get_if<CompilationRef *>();
  assert(CR != nullptr);
  if (P->compare_exchange_weak(Expected, TaggedPointer::Busy,
                               std::memory_order_acq_rel,
                               std::memory_order_relaxed)) {
    Expected = CreateFromCompilationRef(P, CR);
    P->store(Expected, std::memory_order_release);
    return true;
  }
  return false;
}

/// Performs a Symbol* -> Busy -> Symbol* state transition.
///
/// \tparam UpdateFn  A function with signature
/// TaggedPointer(std::atomic<void*>*, Symbol*).
///
/// \param P  A pointer to the atomic to be set. This should lie within the
///   repository shadow memory area.
/// \param [in,out] Expected  On entry, must point to a symbol pointer.
/// \param Update  A function used to update the symbol to which \p Expected
///   points. This function may adjust the body of the symbol or point it to
///   a different symbol instance altogether.
/// \returns  True if the operation was performed, false if retry is necessary.
///
/// \note This function expected to be called from within a loop which checks
///   the value of expected. This enables use of compare_exchange_weak() which
///   may be slightly faster than compare_exchange_strong() alternative on some
///   platforms.
template <typename UpdateFn>
inline bool symbolToFinal(AtomicTaggedPointer *const P, TaggedPointer &Expected,
                          const UpdateFn Update) {
  auto *const Sym = Expected.get_if<Symbol *>();
  assert(Sym != nullptr &&
         "symbolToFinal must be called with a Symbol * in the shadow memory");
  if (P->compare_exchange_weak(Expected, TaggedPointer::Busy,
                               std::memory_order_acq_rel,
                               std::memory_order_relaxed)) {
    Expected = Update(P, Sym);
    P->store(Expected, std::memory_order_release);
    return true;
  }
  return false;
}

/// \param P  A pointer to the atomic to be set. This should lie within the
///   repository shadow memory area.
/// \returns The value contained within the atomic.
inline TaggedPointer spinWhileBusy(AtomicTaggedPointer *const P) {
  auto Expected = TaggedPointer::Busy;
  while ((Expected = P->load(std::memory_order_acquire)) ==
         TaggedPointer::Busy) {
    std::this_thread::yield();
  }
  return Expected;
}

} // end namespace details

/// \tparam CreateFn  A function with signature TaggedPointer().
/// \tparam CreateFromCompilationRefFn  A function with signature
///   TaggedPointer(std::atomic<void*>*, CompilationRef *).
/// \tparam UpdateFn A function with signature
///   TaggedPointer(std::atomic<void*>*, Symbol*). In the event of an error, the
///   function should return TaggedPointer{nullptr}.
///
/// \param P  A pointer to the atomic to be set. This should lie within the
///   repository shadow memory area.
/// \param Create  A function called to create a new Symbol or CompilationRef.
/// \param CreateFromCompilationRef  A function called to update a
///   CompilationRef or to create a Symbol based on the input CompilationRef.
/// \param Update  A function used to update a Symbol. This function may adjust
///   the body of the symbol or point it to a different symbol instance
///   altogether.
template <typename CreateFn, typename CreateFromCompilationRefFn,
          typename UpdateFn>
std::tuple<TaggedPointer, bool>
set(AtomicTaggedPointer *const P, const CreateFn Create,
    const CreateFromCompilationRefFn CreateFromCompilationRef,
    const UpdateFn Update) {

  TaggedPointer Expected;
  if (details::nullToFinal(P, Expected /*in,out*/, Create)) {
    return std::make_tuple(Expected, true);
  }

  for (;;) {
    if (Expected == TaggedPointer::Busy) {
      Expected = details::spinWhileBusy(P);
    }
    if (Expected.get_if<CompilationRef *>() != nullptr) {
      // CompilationRef* -> Busy -> Symbol*/CompilationRef*
      if (details::compilationRefToFinal(P, Expected /*in,out*/,
                                         CreateFromCompilationRef)) {
        return std::make_tuple(Expected, static_cast<bool>(Expected));
      }
    } else {
      // Symbol* -> Busy -> Symbol*
      auto *const OriginalSym = Expected.get_if<Symbol *>();
      assert(OriginalSym != nullptr);
      if (details::symbolToFinal(P, Expected /*in,out*/, Update)) {
        const auto OK = static_cast<bool>(Expected);
        TaggedPointer x = OK ? Expected : TaggedPointer{OriginalSym};
        return std::make_tuple(x, OK);
      }
    }
  }
}

} // end namespace shadow
} // end namespace rld

#endif // RLD_SHADOW_HPP
