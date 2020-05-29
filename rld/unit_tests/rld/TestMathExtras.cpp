#include "rld/MathExtras.h"
#include <gtest/gtest.h>

using namespace rld;

TEST(MathExtras, alignTo) {
  EXPECT_EQ(0U, alignTo(0U, 1U));
  EXPECT_EQ(1U, alignTo(1U, 1U));

  EXPECT_EQ(8U, alignTo(5U, 8U));
  EXPECT_EQ(24U, alignTo(17, 8U));
  EXPECT_EQ(0U, alignTo(~0LL, 8U));

  EXPECT_EQ(4096U, alignTo(4095U, 4096U));
  EXPECT_EQ(4096U, alignTo(4096U, 4096U));
  EXPECT_EQ(8192U, alignTo(4097U, 4096U));
}
