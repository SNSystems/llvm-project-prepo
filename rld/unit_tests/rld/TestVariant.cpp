//===- unit_tests/rld/TestVariant.cpp -------------------------------------===//
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
#include "rld/Variant.h"

#include "gmock/gmock.h"

#include <type_traits>

using namespace rld;

static_assert(
    std::is_same<int, VariantAlternative_t<0, Variant<int, float>>>::value,
    "VariantAlternative<> should produce int at index 0 of [int, float]");
static_assert(
    std::is_same<float, VariantAlternative_t<1, Variant<int, float>>>::value,
    "VariantAlternative<> should produce float at index 1 of [int, float]");
// cv-qualification on the variant type propagates to the extracted alternative
// type.
static_assert(
    std::is_same<int const,
                 VariantAlternative_t<0, Variant<int, float> const>>::value,
    "VariantAlternative<> should produce int const at index 0 of [int, "
    "float]");

struct NoDefaultCtor {
  explicit NoDefaultCtor(int A) : A{A} {}
  int A;
};

TEST(Variant, Ctor) {
  {
    Variant<char, int> V1;
    EXPECT_EQ(V1.index(), 0U);
  }
  {
    Variant<int, NoDefaultCtor> V2;
    EXPECT_EQ(V2.index(), 0U);
  }
  {
    Variant<int, NoDefaultCtor> V2;
    EXPECT_EQ(V2.index(), 0U);
  }
  {
    Variant<char, int> V3{17};
    EXPECT_EQ(V3.index(), 1U);
    EXPECT_EQ(get<int>(V3), 17);
    EXPECT_EQ(get<1>(V3), 17);
  }
  {
    Variant<NoDefaultCtor, int> V4{19};
    EXPECT_EQ(V4.index(), 1U);
    EXPECT_EQ(get<int>(V4), 19);
  }
  {
    Variant<NoDefaultCtor, int> V5{in_place_type_t<NoDefaultCtor>{}, 23};
    EXPECT_EQ(V5.index(), 0U);
    EXPECT_EQ(get<NoDefaultCtor>(V5).A, 23);
  }
  {
    Variant<NoDefaultCtor, int> V6{in_place_index_t<0>{}, 29};
    EXPECT_EQ(V6.index(), 0U);
    EXPECT_EQ(get<NoDefaultCtor>(V6).A, 29);
  }
}

TEST(Variant, Hash) {
  {
    Variant<char, int> V1;
    Variant<char, int> V2{'c'};
    EXPECT_NE(std::hash<decltype(V1)>{}(V1), std::hash<decltype(V2)>{}(V2));
  }
  {
    Variant<char, int> V1{'a'};
    Variant<char, int> V2{'b'};
    EXPECT_NE(std::hash<decltype(V1)>{}(V1), std::hash<decltype(V2)>{}(V2));
  }
  {
    Variant<char, int> V1{'a'};
    Variant<char, int> V2{static_cast<int>('a')};
    EXPECT_NE(std::hash<decltype(V1)>{}(V1), std::hash<decltype(V2)>{}(V2));
  }
  {
    Variant<int, char> V1{'a'};
    Variant<int, char> V2{static_cast<int>('a')};
    EXPECT_NE(std::hash<decltype(V1)>{}(V1), std::hash<decltype(V2)>{}(V2));
  }
}

TEST(Variant, Assign) {
  Variant<char, int> V;
  V = 10;
  {
    auto *const I = getIf<int>(&V);
    ASSERT_NE(I, nullptr);
    EXPECT_EQ(*I, 10);
  }
  V = 'a';
  {
    auto *const C = getIf<char>(&V);
    ASSERT_NE(C, nullptr);
    EXPECT_EQ(*C, 'a');
  }
}

TEST(Variant, Emplace) {
  Variant<char, int> V;
  {
    int &RI = V.emplace<int>(10);
    EXPECT_EQ(RI, 10);
    int *const I = getIf<int>(&V);
    ASSERT_NE(I, nullptr);
    EXPECT_EQ(*I, 10);
  }
  {
    char &C = V.emplace<char>('c');
    EXPECT_EQ(C, 'c');
    EXPECT_EQ(getIf<int>(&V), nullptr);
    EXPECT_NE(getIf<char>(&V), nullptr);
    EXPECT_EQ(get<char>(V), 'c');
  }
}

TEST(Variant, GetIf1) {
  Variant<char, int> V{'c'};
  auto *const P = getIf<int>(&V);
  assert(P == nullptr);
  char *const C = getIf<char>(&V);
  assert(C != nullptr);
  assert(get<char>(V) == 'c');
}

TEST(Variant, GetIf2) {
  Variant<char, int> V{17};
  auto *const C = getIf<char>(&V);
  assert(C == nullptr);
  int *const P = getIf<int>(&V);
  assert(P != nullptr);
  assert(get<int>(V) == 17);
}

TEST(Variant, OperatorEq) {
  Variant<char, int> V1{17};
  Variant<char, int> V2{17};
  EXPECT_TRUE(V1 == V2);
  Variant<char, int> V3{19};
  EXPECT_FALSE(V1 == V3);
  EXPECT_TRUE(V1 != V3);
}

TEST(Variant, HoldsAlternative) {
  Variant<char, int> V{'c'};
  EXPECT_TRUE(holdsAlternative<char>(V));
  EXPECT_FALSE(holdsAlternative<int>(V));
}
