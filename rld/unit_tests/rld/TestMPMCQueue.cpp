//===- unit_tests/rld/TestMPMCQueue.cpp -----------------------------------===//
//*  __  __ ____  __  __  ____ ___                         *
//* |  \/  |  _ \|  \/  |/ ___/ _ \ _   _  ___ _   _  ___  *
//* | |\/| | |_) | |\/| | |  | | | | | | |/ _ \ | | |/ _ \ *
//* | |  | |  __/| |  | | |__| |_| | |_| |  __/ |_| |  __/ *
//* |_|  |_|_|   |_|  |_|\____\__\_\\__,_|\___|\__,_|\___| *
//*                                                        *
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
/*
Copyright (c) 2018 Erik Rigtorp <erik@rigtorp.se>
Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
 */
#include "rld/MPMCQueue.h"

#include "llvm/ADT/DenseSet.h"

#include "gmock/gmock.h"

#include <memory>
#include <thread>
#include <vector>

namespace {

class MPMCQueueMemberTracking : public ::testing::Test {
protected:
  struct Counter {
    explicit Counter(MPMCQueueMemberTracking *const Host) noexcept
        : Host_{Host} {
      Host_->Constructed.insert(this);
    }

    Counter(Counter const &Other) noexcept : Host_{Other.Host_} {
      EXPECT_EQ(Host_->Constructed.count(this), 0U);
      EXPECT_EQ(Host_->Constructed.count(&Other), 1U);
      Host_->Constructed.insert(this);
    }

    Counter(Counter &&Other) noexcept : Host_{Other.Host_} {
      EXPECT_EQ(Host_->Constructed.count(this), 0U);
      EXPECT_EQ(Host_->Constructed.count(&Other), 1U);
      Host_->Constructed.insert(this);
    }

    Counter &operator=(Counter const &Other) noexcept {
      if (&Other != this) {
        Host_ = Other.Host_;
        EXPECT_EQ(Host_->Constructed.count(this), 1U);
        EXPECT_EQ(Host_->Constructed.count(&Other), 1U);
      }
      return *this;
    }

    Counter &operator=(Counter &&Other) noexcept {
      if (&Other != this) {
        Host_ = Other.Host_;
        EXPECT_EQ(Host_->Constructed.count(this), 1U);
        EXPECT_EQ(Host_->Constructed.count(&Other), 1U);
      }
      return *this;
    }

    ~Counter() noexcept {
      EXPECT_EQ(Host_->Constructed.count(this), 1U);
      Host_->Constructed.erase(this);
    }

    MPMCQueueMemberTracking *Host_;
    // To verify that alignment and padding calculations are handled correctly
    char Data[129];
  };

  llvm::DenseSet<Counter const *> Constructed;
};

} // end anonymous namespace

using namespace rld;

TEST_F(MPMCQueueMemberTracking, ObjectCounts) {
  {
    constexpr auto Entries = 10U;

    MPMCQueue<Counter> Q{11};
    for (auto Ctr = 0U; Ctr < Entries; Ctr++) {
      Q.emplace(this);
    }
    EXPECT_EQ(Constructed.size(), Entries);

    {
      auto const T1 = Q.pop();
      EXPECT_EQ(Constructed.size(), Entries);
    }
    {
      auto const T2 = Q.pop();
      Q.emplace(this);
      EXPECT_EQ(Constructed.size(), Entries);
    }
  }
  EXPECT_EQ(Constructed.size(), 0U);
}

TEST(MPMCQueue, TryPushTryPop) {
  MPMCQueue<int> Q{1};
  EXPECT_TRUE(Q.try_push(1));
  EXPECT_FALSE(Q.try_push(2));
  llvm::Optional<int> T1 = Q.try_pop();
  ASSERT_TRUE(T1);
  EXPECT_EQ(*T1, 1);
  llvm::Optional<int> T2 = Q.try_pop();
  ASSERT_FALSE(T2);
}

TEST(MPMCQueue, CopyableOnlyType) {
  struct CopyableOnly {
    CopyableOnly() noexcept = default;
    CopyableOnly(CopyableOnly const &) noexcept = default;
    CopyableOnly(CopyableOnly &&) = delete;

    CopyableOnly &operator=(CopyableOnly const &) noexcept { return *this; }
    CopyableOnly &operator=(CopyableOnly &&) noexcept = delete;
  };

  MPMCQueue<CopyableOnly> q{16U};
  // lvalue
  CopyableOnly v;
  q.emplace(v);
  q.try_emplace(v);
  q.push(v);
  q.try_push(v);
  // xvalue
  q.push(CopyableOnly{});
  q.try_push(CopyableOnly{});
}

TEST(MPMCQueue, MovableOnlyType) {
  MPMCQueue<std::unique_ptr<int>> q{16U};
  // lvalue
  // auto v = uptr(new int(1));
  // q.emplace(v);
  // q.try_emplace(v);
  // q.push(v);
  // q.try_push(v);
  // xvalue
  q.emplace(std::make_unique<int>(1));
  q.try_emplace(std::make_unique<int>(1));
  q.push(std::make_unique<int>(1));
  q.try_push(std::make_unique<int>(1));
}

// Work around a bug in cl.exe which does not implicitly capture constexpr
// values.
#ifdef _MSC_VER
#define CAPTURE_BUG(...) , __VA_ARGS__
#else
#define CAPTURE_BUG(...)
#endif

TEST(MPMCQueue, Fuzz) {
  constexpr auto NumOps = 1000ULL;
  constexpr auto NumThreads = 10U;
  MPMCQueue<unsigned long long> Q{NumThreads};
  std::atomic<bool> Flag{false};
  std::vector<std::thread> Threads;
  std::atomic<unsigned long long> Sum{0ULL};
  for (auto Ctr = 0U; Ctr < NumThreads; ++Ctr) {
    Threads.emplace_back(
        [&Q, &Flag CAPTURE_BUG(NumOps, NumThreads)](unsigned const TCtr) {
          while (!Flag) {
          }
          for (unsigned long long J = TCtr; J < NumOps; J += NumThreads) {
            Q.push(J);
          }
        },
        Ctr);
  }

  for (auto Ctr = 0U; Ctr < NumThreads; ++Ctr) {
    Threads.emplace_back(
        [&Q, &Flag, &Sum CAPTURE_BUG(NumOps, NumThreads)](unsigned const TCtr) {
          while (!Flag) {
          }
          auto ThreadSum = 0ULL;
          for (auto J = TCtr; J < NumOps; J += NumThreads) {
            ThreadSum += Q.pop();
          }
          Sum += ThreadSum;
        },
        Ctr);
  }
  Flag = true;
  for (auto &Thread : Threads) {
    Thread.join();
  }
  ASSERT_EQ(Sum, NumOps * (NumOps - 1) / 2);
}

#undef CAPTURE_BUG
