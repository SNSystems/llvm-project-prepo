#include "rld/CSAlloc.h"

#include "gtest/gtest.h"

TEST(CSAlloc, TwoElementsTwice) {
  pstore::chunked_sequence<uint8_t, 3 * sizeof(int)> cv;
  {
    int *p1 = rld::csAlloc<int>(&cv, 2);
    ASSERT_EQ(reinterpret_cast<uintptr_t>(p1) % alignof(int), 0U);
    p1[0] = 3;
    p1[1] = 5;
  }
  {
    int *p2 = rld::csAlloc<int>(&cv, 2);
    ASSERT_EQ(reinterpret_cast<uintptr_t>(p2) % alignof(int), 0U);
    p2[0] = 7;
    p2[1] = 11;
  }
  EXPECT_GE(cv.size(), 5U);
}

TEST(CSAlloc, OneElementThenThree) {
  pstore::chunked_sequence<uint8_t, 3 * sizeof(int)> cv;
  {
    int *p1 = rld::csAlloc<int>(&cv, 1);
    ASSERT_EQ(reinterpret_cast<uintptr_t>(p1) % alignof(int), 0U);
    *p1 = 3;
  }
  {
    // A request for three contiguous elements when the container. These should
    // occupy the second chunk, with the remaining unoccupied elements of the
    // first chunk being filled with nullptr.
    int *p2 = rld::csAlloc<int>(&cv, 3);
    ASSERT_EQ(reinterpret_cast<uintptr_t>(p2) % alignof(int), 0U);
    p2[0] = 5;
    p2[1] = 7;
    p2[2] = 11;
  }
  EXPECT_EQ(cv.size(), 3 * sizeof(int) * 2);
}
