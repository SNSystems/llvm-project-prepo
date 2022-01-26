//===- include/rld/TypeList.h -----------------------------*- mode: C++ -*-===//
//*  _____                   _     _     _    *
//* |_   _|   _ _ __   ___  | |   (_)___| |_  *
//*   | || | | | '_ \ / _ \ | |   | / __| __| *
//*   | || |_| | |_) |  __/ | |___| \__ \ |_  *
//*   |_| \__, | .__/ \___| |_____|_|___/\__| *
//*       |___/|_|                            *
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#ifndef RLD_TYPE_LIST_H
#define RLD_TYPE_LIST_H

#include <limits>
#include <type_traits>

namespace rld {
namespace type_list {

struct BadType {};
using IndexType = unsigned;
constexpr auto BadIndex = std::numeric_limits<IndexType>::max();

namespace details {

// type to index helper
// ~~~~~~~~~~~~~~~~~~~~
template <IndexType Index, typename T, typename... TypeList>
struct TypeToIndexHelper;

template <IndexType Index, typename T> struct TypeToIndexHelper<Index, T> {
  static constexpr auto value =
      BadIndex; // NOLINT(readability-identifier-naming)
};

template <IndexType Index, typename T, typename Member1, typename... Others>
struct TypeToIndexHelper<Index, T, Member1, Others...> {
  // NOLINTNEXTLINE(readability-identifier-naming)
  static constexpr int value =
      std::is_same<T, Member1>::value
          ? Index
          : TypeToIndexHelper<Index + IndexType{1}, T, Others...>::value;
};

// index to type helper
// ~~~~~~~~~~~~~~~~~~~~
template <IndexType Index, typename... TypeList> struct IndexToTypeHelper;
template <IndexType Index> struct IndexToTypeHelper<Index> {
  using type = BadType;
};
template <IndexType Index, typename Member1, typename... Others>
struct IndexToTypeHelper<Index, Member1, Others...> {
  using type = std::conditional_t<
      (Index > IndexType{0}),
      typename IndexToTypeHelper<Index - IndexType{1}, Others...>::type,
      Member1>;
};

// front helper
// ~~~~~~~~~~~~
template <typename T, typename... Ts> struct FrontHelper { using type = T; };

} // end namespace details

// size
// ~~~~
/// Yields the number of types in the list TypeList.
template <typename... TypeList> struct Size {
  // NOLINTNEXTLINE(readability-identifier-naming)
  static constexpr auto value = sizeof...(TypeList);
};

// is one of
// ~~~~~~~~~
/// Produces a value of true if type T is one of the types in TypeList or false
/// if it is not.
template <typename T, typename... TypeList> struct IsOneOf;
template <typename T> struct IsOneOf<T> {
  // NOLINTNEXTLINE(readability-identifier-naming)
  static constexpr bool value = false;
};
template <typename T, typename Member1, typename... Others>
struct IsOneOf<T, Member1, Others...> {
  // NOLINTNEXTLINE(readability-identifier-naming)
  static constexpr bool value =
      std::is_same<T, Member1>::value || IsOneOf<T, Others...>::value;
};

// all of
// ~~~~~~
/// Checks if the predicate template type P yields true for all types in the
/// list TypeList.
template <template <typename Ty> class P, typename... TypeList> struct AllOf;
template <template <typename Ty> class P, typename T> struct AllOf<P, T> {
  // NOLINTNEXTLINE(readability-identifier-naming)
  static constexpr bool value = P<T>::value;
};
template <template <typename Ty> class P, typename T, typename... Ts>
struct AllOf<P, T, Ts...> {
  // NOLINTNEXTLINE(readability-identifier-naming)
  static constexpr bool value = P<T>::value && AllOf<P, Ts...>::value;
};

// any of
// ~~~~~~
/// Checks if the predicate template type P yields true for any types in the
/// list TypeList.
template <template <typename Ty> class P, typename... TypeList> struct AnyOf;
template <template <typename Ty> class P, typename T> struct AnyOf<P, T> {
  // NOLINTNEXTLINE(readability-identifier-naming)
  static constexpr bool value = P<T>::value;
};
template <template <typename Ty> class P, typename T, typename... Ts>
struct AnyOf<P, T, Ts...> {
  // NOLINTNEXTLINE(readability-identifier-naming)
  static constexpr bool value = P<T>::value || AnyOf<P, Ts...>::value;
};

// type to index
// ~~~~~~~~~~~~~
/// Yields the index of type T within the list of types given by TypeList or
/// BadIndex if T is not found in the list.
template <typename T, typename... TypeList> struct TypeToIndex {
  // NOLINTNEXTLINE(readability-identifier-naming)
  static constexpr IndexType value =
      details::TypeToIndexHelper<0U, T, TypeList...>::value;
};

// index to type
// ~~~~~~~~~~~~~
/// Yields the type at index Index within the list of types given by TypeList or
/// BadType if Index lies beyond the length of the list.
template <IndexType Index, typename... TypeList> struct IndexToType {
  using type = typename details::IndexToTypeHelper<Index, TypeList...>::type;
};
/// Yields the type at index Index within the list of types given by TypeList or
/// BadType if Index lies beyond the length of the list.
template <IndexType Index, typename... TypeList>
using IndexToType_t = typename IndexToType<Index, TypeList...>::type;

// front
// ~~~~~
template <typename... TypeList> struct Front {
  using type = typename details::FrontHelper<TypeList...>::type;
};
template <typename... TypeList>
using Front_t = typename Front<TypeList...>::type;

} // end namespace type_list
} // end namespace rld

// Tests

static_assert(rld::type_list::Size<char, int>::value == 2U,
              "the size of list [char, int] should be 2");
static_assert(rld::type_list::Size<char>::value == 1U,
              "the size of list [char] should be 1");
static_assert(rld::type_list::Size<>::value == 0U,
              "the size of the empty list should be 0");

static_assert(rld::type_list::IsOneOf<long, char, int, long>::value,
              "IsOneOf<> should yield true because long is in the list "
              "[char, int, long]");
static_assert(
    rld::type_list::IsOneOf<int, char, int>::value,
    "IsOneOf<> should be true because int is in the list [char, int]");
static_assert(
    !rld::type_list::IsOneOf<int, char>::value,
    "IsOneOf<> should be false because int is not in the list [char]");
static_assert(
    !rld::type_list::IsOneOf<int, char, long>::value,
    "IsOneOf<> should be false because int is not in the list [char, long]");
static_assert(!rld::type_list::IsOneOf<int>::value,
              "IsOneOf<> should be false because int is not in the empty list");

static_assert(rld::type_list::TypeToIndex<int, char, int>::value == 1U,
              "TypeToIndex<> should return 1 because int is the second "
              "element in [char, int]");
static_assert(rld::type_list::TypeToIndex<char, char, int>::value == 0U,
              "TypeToIndex<> should return 0 because char is the first "
              "element in [char, int]");
static_assert(rld::type_list::TypeToIndex<char>::value ==
                  rld::type_list::BadIndex,
              "TypeToIndex<> should return BadIndex because char is not in "
              "the empty list");
static_assert(rld::type_list::TypeToIndex<char, int, long>::value ==
                  rld::type_list::BadIndex,
              "TypeToIndex<> shoiuld return BadIndex because char is not in "
              "list [int, long]");

static_assert(
    std::is_same<rld::type_list::IndexToType_t<0U, char, int>, char>::value,
    "IndexToType_t<> should yield char because this is at index 0 in the "
    "list [char, int]");
static_assert(
    std::is_same<rld::type_list::IndexToType_t<1U, char, int>, int>::value,
    "IndexToType_t<> should yield int because this is at index 1 in the list "
    "[char, int]");
static_assert(std::is_same<rld::type_list::IndexToType_t<2U, char, int>,
                           rld::type_list::BadType>::value,
              "IndexToType_t<> should yield BadType becuase there is no "
              "type at index 2 in the "
              "list [char, int]");

static_assert(std::is_same<rld::type_list::Front_t<int, char>, int>::value,
              "front_t<> should yield int as the first of [int, char]");

#endif // RLD_TYPE_LIST_H
