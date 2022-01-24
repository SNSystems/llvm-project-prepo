//===- include/rld/Variant.h ------------------------------*- mode: C++ -*-===//
//* __     __         _             _    *
//* \ \   / /_ _ _ __(_) __ _ _ __ | |_  *
//*  \ \ / / _` | '__| |/ _` | '_ \| __| *
//*   \ V / (_| | |  | | (_| | | | | |_  *
//*    \_/ \__,_|_|  |_|\__,_|_| |_|\__| *
//*                                      *
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#ifndef RLD_VARIANT_H
#define RLD_VARIANT_H

#include "rld/TypeList.h"

#include <cassert>
#include <functional>
#include <new>
#include <utility>

namespace rld {

// This type is emulating std::in_place_t from C++17, so we use the standard's
// naming convention.
// NOLINTNEXTLINE(readability-identifier-naming)
struct in_place_t {
  explicit in_place_t() = default;
};
// This type is emulating std::in_place_type_t from C++17, so we use the
// standard's naming convention.
// NOLINTNEXTLINE(readability-identifier-naming)
template <typename T> struct in_place_type_t {
  explicit in_place_type_t() = default;
};
// This type is emulating std::in_place_index_t from C++17, so we use the
// standard's naming convention.
// NOLINTNEXTLINE(readability-identifier-naming)
template <std::size_t I> struct in_place_index_t {
  explicit in_place_index_t() = default;
};

// Inline variables are not supported in C++14, so we don't have the following
// definitions from C++17: inline constexpr in_place_t in_place{}; template
// <typename T> inline constexpr in_place_type_t<T> in_place_type{}; template
// <std::size_t I> inline constexpr in_place_index_t<I> in_place_index{};

namespace variant_details {

// max
// ~~~
template <size_t V, size_t... Others> struct Max;
template <size_t V> struct Max<V> {
  // NOLINTNEXTLINE(readability-identifier-naming)
  static constexpr size_t value = V;
};
template <size_t V1, size_t V2, size_t... Others>
struct Max<V1, V2, Others...> {
  // NOLINTNEXTLINE(readability-identifier-naming)
  static constexpr size_t value =
      V1 > V2 ? Max<V1, Others...>::value : Max<V2, Others...>::value;
};

static_assert(Max<1, 2, 5, 4, 3>::value == 5, "max<> result should be 5");
static_assert(Max<1>::value == 1, "max<> result should be 1");

template <typename... Ts> struct VariantHelper;

template <typename T, typename... Ts> struct VariantHelper<T, Ts...> {
  using TPointer = std::add_pointer_t<T>;
  using TConstPointer = std::add_pointer_t<T const>;

  template <typename Storage>
  static bool eq(type_list::IndexType const Index, Storage const &LHS,
                 Storage const &RHS) {
    if (Index == 0U) {
      return *reinterpret_cast<TConstPointer>(&LHS) ==
             *reinterpret_cast<TConstPointer>(&RHS);
    }
    return VariantHelper<Ts...>::eq(Index - type_list::IndexType{1}, LHS, RHS);
  }

  template <typename Storage>
  static size_t hash(type_list::IndexType const Index, Storage const &S) {
    if (Index == 0U) {
      return std::hash<T>{}(*reinterpret_cast<TConstPointer>(&S));
    }
    return VariantHelper<Ts...>::hash(Index - type_list::IndexType{1}, S);
  }

  template <typename Storage>
  static void destroy(type_list::IndexType const Index, Storage *const S) {
    if (Index == 0U) {
      reinterpret_cast<TPointer>(S)->~T();
      return;
    }
    VariantHelper<Ts...>::destroy(Index - type_list::IndexType{1}, S);
  }

  template <typename Storage>
  static void copy(type_list::IndexType const Index, Storage *const To,
                   Storage const *const From) {
    if (Index == 0U) {
      new (To) T(*reinterpret_cast<TConstPointer>(From));
      return;
    }
    VariantHelper<Ts...>::copy(Index - type_list::IndexType{1}, To, From);
  }

  template <typename Storage>
  static void move(type_list::IndexType const Index, Storage *const To,
                   Storage *const From) {
    if (Index == 0U) {
      new (To) T(std::move(*reinterpret_cast<TPointer>(From)));
      return;
    }
    VariantHelper<Ts...>::move(Index - type_list::IndexType{1}, To, From);
  }
};

template <> struct VariantHelper<> {
  template <typename Storage>
  static bool eq(type_list::IndexType, Storage const &, Storage const &) {
    return false;
  }
  template <typename Storage>
  static size_t hash(type_list::IndexType, Storage const &) {
    return 0;
  }
  template <typename Storage>
  static void destroy(type_list::IndexType, Storage *) {}
  template <typename Storage>
  static void move(type_list::IndexType, Storage *, Storage *) {}
  template <typename Storage>
  static void copy(type_list::IndexType, Storage *, Storage const *) {}
};

template <class T, bool> struct DependentType : public T {};

} // end namespace variant_details

template <typename... Types> class Variant;

constexpr std::size_t VariantNPos = -1;

//*               _          _          _ _                     _   _          *
//* __ ____ _ _ _(_)__ _ _ _| |_   __ _| | |_ ___ _ _ _ _  __ _| |_(_)_ _____  *
//* \ V / _` | '_| / _` | ' \  _| / _` | |  _/ -_) '_| ' \/ _` |  _| \ V / -_) *
//*  \_/\__,_|_| |_\__,_|_||_\__| \__,_|_|\__\___|_| |_||_\__,_|\__|_|\_/\___| *
//*                                                                            *
template <size_t _Ip, class _Tp> struct VariantAlternative;

template <std::size_t I, typename Variant>
using VariantAlternative_t = typename VariantAlternative<I, Variant>::type;

template <size_t _Ip, class _Tp>
struct VariantAlternative<_Ip, const _Tp>
    : std::add_const<VariantAlternative_t<_Ip, _Tp>> {};

template <size_t _Ip, class _Tp>
struct VariantAlternative<_Ip, volatile _Tp>
    : std::add_volatile<VariantAlternative_t<_Ip, _Tp>> {};

template <size_t _Ip, class _Tp>
struct VariantAlternative<_Ip, const volatile _Tp>
    : std::add_cv<VariantAlternative_t<_Ip, _Tp>> {};

template <size_t I, class... Types>
struct VariantAlternative<I, Variant<Types...>> {
  static_assert(I < sizeof...(Types),
                "Index out of bounds in std::variant_alternative<>");
  using type = typename type_list::IndexToType_t<I, Types...>;
};

namespace variant_details {

// get impl
// ~~~~~~~~
template <size_t I, typename Variant> constexpr auto &&getImpl(Variant &&V) {
#if EXCEPTIONS
  if (V.index() != I) {
    throw bad_variant_access{};
  }
#else
  assert(V.index() == I);
#endif // EXCEPTIONS
  return *reinterpret_cast<std::add_pointer_t<
      VariantAlternative_t<I, typename std::remove_reference<Variant>::type>>>(
      &V.storage());
}

// get if impl
// ~~~~~~~~~~~
template <std::size_t I, typename Variant>
constexpr std::add_pointer_t<VariantAlternative_t<I, Variant>>
getIfImpl(Variant *const PV) noexcept {
  return PV->index() == I ? &getImpl<I>(*PV) : nullptr;
}

// combine
// ~~~~~~~
/// Used to concatenate two hash values.
constexpr size_t combine(const size_t A, const size_t B) noexcept {
  return (23U + A) * 37U + B;
}

// hash impl
// ~~~~~~~~~
template <typename Variant> size_t hashImpl(Variant &&V) {
  if (V.valuelessByException()) {
    return 2174902584225795; // Where the dice will stop, noone knows.
  }
  return combine(
      std::hash<decltype(V.Holds)>{}(V.Holds),
      std::remove_reference_t<Variant>::helper::hash(V.Holds, V.storage()));
}

} // end namespace variant_details

//*               _          _    *
//* __ ____ _ _ _(_)__ _ _ _| |_  *
//* \ V / _` | '_| / _` | ' \  _| *
//*  \_/\__,_|_| |_\__,_|_||_\__| *
//*                               *
/// \tparam Types The types that may be stored in this variant.
///
/// The class template variant represents a type-safe union. An instance of
/// variant at any given time either holds a value of one of its alternative
/// types. valueless_by_exception).
template <typename... Types> class Variant {
  using first_type = VariantAlternative_t<0, Variant>;
  using helper = variant_details::VariantHelper<Types...>;

public:
  /// Default constructor. Constructs a variant holding the value-initialized
  /// value of the first alternative type.
  template <bool Dummy = true,
            std::enable_if_t<
                variant_details::DependentType<
                    std::is_default_constructible<first_type>, Dummy>::value,
                int> = 0>
  constexpr Variant() noexcept(
      std::is_nothrow_default_constructible<first_type>::value)
      : Holds{type_list::TypeToIndex<first_type, Types...>::value} {
    new (&Storage) first_type;
  }

  /// Constructs a variant with the alternative type specified by the index I
  /// and initializes the contained value with the arguments
  /// std::forward<Args>(args)....
  template <std::size_t I, typename... Args>
  constexpr explicit Variant(in_place_index_t<I>, Args &&...Argv) : Holds{I} {
    static_assert(I < sizeof...(Args),
                  "I must not be greater than the number of types");
    new (&Storage) VariantAlternative_t<I, Variant<Types...>>(
        std::forward<Args...>(Argv)...);
  }

  /// Constructs a variant with the specified alternative T and initializes the
  /// contained value with the arguments std::forward<Args>(args)....
  template <typename T, typename... Args>
  constexpr explicit Variant(in_place_type_t<T>, Args &&...Argv)
      : Holds{type_list::TypeToIndex<T, Types...>::value} {
    new (&Storage) T(std::forward<Args...>(Argv)...);
  }

  template <typename T, typename = typename std::enable_if_t<
                            type_list::IsOneOf<T, Types...>::value>>
  explicit constexpr Variant(T &&Other)
      : Holds{type_list::TypeToIndex<T, Types...>::value} {
    new (&Storage) T(std::forward<T>(Other));
  }
  Variant(Variant const &Other) : Holds{Other.Holds} {
    helper::copy(Other.Holds, &this->storage(), &Other.storage());
  }
  Variant(Variant &&Other) noexcept : Holds{Other.Holds} {
    helper::move(Other.Holds, &this->storage(), &Other.storage());
  }

  ~Variant() noexcept;

  template <typename T, typename = typename std::enable_if_t<
                            type_list::IsOneOf<T, Types...>::value>>
  Variant &operator=(T &&Other);

  Variant &operator=(Variant const &Other);
  Variant &operator=(Variant &&Other);

  /// Creates a new value in-place, in an existing variant object.
  /// \param Argv  Constructor arguments to use when constructing the new value.
  template <typename T, typename... Args> T &emplace(Args &&...Argv);

  /// Creates a new value in-place, in an existing variant object.
  /// \param Argv  CVonstructor arguments to use when constructing the new
  /// value.
  template <std::size_t I, class... Args>
  VariantAlternative_t<I, Variant> &emplace(Args &&...Argv);

  /// Returns the zero-based index of the alternative that is currently held by
  /// the variant. If the variant is valueless_by_exception, returns
  /// VariantNPos.
  constexpr std::size_t index() const noexcept {
    return Holds == type_list::BadIndex ? VariantNPos : Holds;
  }
  /// Returns false if and only if the variant holds a value.
  constexpr bool valuelessByException() const noexcept {
    return Holds == type_list::BadIndex;
  }

private:
  template <size_t I, typename Variant>
  friend constexpr auto &&variant_details::getImpl(Variant &&V);

  template <typename Variant>
  friend size_t variant_details::hashImpl(Variant &&V);

  template <typename... TypeList>
  friend constexpr bool operator==(Variant<TypeList...> const &LHS,
                                   Variant<TypeList...> const &RHS);

  void destroy();

  auto &storage() noexcept { return Storage; }
  auto const &storage() const noexcept { return Storage; }

  type_list::IndexType Holds = type_list::BadIndex;
  typename std::aligned_storage_t<
      variant_details::Max<sizeof(Types)...>::value,
      variant_details::Max<alignof(Types)...>::value>
      Storage;
};

// (dtor)
// ~~~~~~
template <typename... Types> Variant<Types...>::~Variant() noexcept {
  this->destroy();
}

// operator=
// ~~~~~~~~~
template <typename... Types>
template <typename T, typename>
Variant<Types...> &Variant<Types...>::operator=(T &&Other) {
  this->destroy();
  new (&Storage) T(std::forward<T>(Other));
  Holds = type_list::TypeToIndex<T, Types...>::value;
  return *this;
}

template <typename... Types>
Variant<Types...> &Variant<Types...>::operator=(Variant const &Other) {
  if (&Other != this) {
    this->destroy();
    helper::copy(Other.Holds_, &this->storage(), &Other.storage());
  }
  return *this;
}

template <typename... Types>
Variant<Types...> &Variant<Types...>::operator=(Variant &&Other) {
  if (&Other != this) {
    this->destroy();
    helper::move(Other.Holds_, &this->storage(), &Other.storage());
  }
  return *this;
}

// emplace
// ~~~~~~~
template <typename... Types>
template <std::size_t I, class... Args>
VariantAlternative_t<I, Variant<Types...>> &
Variant<Types...>::emplace(Args &&...Argv) {
  using T = VariantAlternative_t<I, Variant<Types...>>;
  this->destroy();
  T *const Ptr = new (&Storage) T(std::forward<Args>(Argv)...);
  Holds = I;
  return *Ptr;
}

template <typename... Types>
template <typename T, typename... Args>
T &Variant<Types...>::emplace(Args &&...Argv) {
  return this->emplace<type_list::TypeToIndex<T, Types...>::value>(
      std::forward<Args>(Argv)...);
}

// destroy
// ~~~~~~~
template <typename... Types> void Variant<Types...>::destroy() {
  if (Holds != type_list::BadIndex) {
    helper::destroy(Holds, &this->storage());
    Holds = type_list::BadIndex;
  }
}

// operator==
// ~~~~~~~~~~
template <typename... Types>
constexpr bool operator==(Variant<Types...> const &LHS,
                          Variant<Types...> const &RHS) {
  if (LHS.index() != RHS.index()) {
    return false;
  }
  if (LHS.valuelessByException()) {
    return true;
  }
  return variant_details::VariantHelper<Types...>::eq(LHS.Holds, LHS.storage(),
                                                      RHS.storage());
}

// operator!=
// ~~~~~~~~~~
template <typename... Types>
constexpr bool operator!=(Variant<Types...> const &LHS,
                          Variant<Types...> const &RHS) {
  return !operator==(LHS, RHS);
}

// holds alternative
// ~~~~~~~~~~~~~~~~~
/// Checks if the variant v holds the alternative T. The call is ill-formed if T
/// does not appear exactly once in Types... \param V The variant to examine.
/// \returns true if the variant currently holds the alternative T, false
/// otherwise.
template <typename T, typename... Types>
constexpr bool holdsAlternative(Variant<Types...> const &V) noexcept {
  return V.index() == type_list::TypeToIndex<T, Types...>::value;
}

#if EXCEPTIONS
// bad variant access
// ~~~~~~~~~~~~~~~~~~
class bad_variant_access : public std::exception {
public:
  virtual char const *what() const noexcept { return "bad variant access"; }
};
#endif // EXCEPTIONS

// get<I, ...TypeList>
// ~~~~~~~~~~~~~~~~~~~
template <std::size_t I, typename... TypeList>
constexpr VariantAlternative_t<I, Variant<TypeList...>> &
get(Variant<TypeList...> &V) {
  return variant_details::getImpl<I>(V);
}
template <std::size_t I, typename... TypeList>
constexpr VariantAlternative_t<I, Variant<TypeList...>> &&
get(Variant<TypeList...> &&V) {
  return variant_details::getImpl<I>(V);
}
template <std::size_t I, typename... TypeList>
constexpr VariantAlternative_t<I, Variant<TypeList...>> &
get(Variant<TypeList...> const &V) {
  return variant_details::getImpl<I>(V);
}
template <std::size_t I, typename... TypeList>
constexpr VariantAlternative_t<I, Variant<TypeList...>> const &&
get(Variant<TypeList...> const &&V) {
  return variant_details::getImpl<I>(V);
}

// get<T, ...TypeList>
// ~~~~~~~~~~~~~~~~~~~
template <typename T, typename... TypeList>
constexpr T &get(Variant<TypeList...> &V) {
  return variant_details::getImpl<
      type_list::TypeToIndex<T, TypeList...>::value>(V);
}
template <typename T, typename... TypeList>
constexpr T &&get(Variant<TypeList...> &&V) {
  return variant_details::getImpl<
      type_list::TypeToIndex<T, TypeList...>::value>(V);
}
template <typename T, typename... TypeList>
constexpr T const &get(Variant<TypeList...> const &V) {
  return variant_details::getImpl<
      type_list::TypeToIndex<T, TypeList...>::value>(V);
}
template <typename T, typename... TypeList>
constexpr T const &&get(Variant<TypeList...> const &&V) {
  return variant_details::getImpl<
      type_list::TypeToIndex<T, TypeList...>::value>(V);
}

// get if<I, ...TypeList>
// ~~~~~~~~~~~~~~~~~~~~~~
template <std::size_t I, typename... Types>
constexpr std::add_pointer_t<VariantAlternative_t<I, Variant<Types...>>>
getIf(Variant<Types...> *const PV) noexcept {
  return PV->index() == I
             ? reinterpret_cast<
                   std::add_pointer_t<type_list::IndexToType_t<I, Types...>>>(
                   &PV->storage())
             : nullptr;
}

template <std::size_t I, typename... Types>
constexpr std::add_pointer_t<const VariantAlternative_t<I, Variant<Types...>>>
getIf(Variant<Types...> const *const PV) noexcept {
  return PV->index() == I
             ? reinterpret_cast<
                   std::add_pointer_t<type_list::IndexToType_t<I, Types...>>>(
                   &PV->storage())
             : nullptr;
}

// get if<T, ...TypeList>
// ~~~~~~~~~~~~~~~~~~~~~~
template <typename T, typename... Types>
constexpr std::add_pointer_t<T> getIf(Variant<Types...> *const PV) noexcept {
  return variant_details::getIfImpl<type_list::TypeToIndex<T, Types...>::value>(
      PV);
}

template <typename T, typename... Types>
constexpr std::add_pointer_t<T const>
getIf(Variant<Types...> const *const PV) noexcept {
  return variant_details::getIfImpl<type_list::TypeToIndex<T, Types...>::value>(
      PV);
}

} // end namespace rld

//*  _            _     *
//* | |_  __ _ __| |_   *
//* | ' \/ _` (_-< ' \  *
//* |_||_\__,_/__/_||_| *
//*                     *
template <typename... Types> struct std::hash<rld::Variant<Types...>> {
  using argument_type = rld::Variant<Types...>;
  using result_type = size_t;
  result_type operator()(argument_type const &V) const {
    return rld::variant_details::hashImpl(V);
  }
};

#endif // RLD_VARIANT_H
