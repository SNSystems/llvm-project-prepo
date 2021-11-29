#include "rld/SymbolEmitList.h"

#include "gmock/gmock.h"

namespace {

struct TestSymbol {
  virtual ~TestSymbol() = default;
  virtual bool setNextEmit(TestSymbol *const Next) = 0;

  bool setNextEmitImpl(TestSymbol *const Next) {
    NextEmit = Next;
    return true;
  }

  TestSymbol *NextEmit = nullptr;
};

class MockTestSymbol : public TestSymbol {
public:
  MOCK_METHOD1(setNextEmit, bool(TestSymbol *const));
};

} // end anonymous namespace

TEST(EmitList, InitialState) {
  rld::EmitList<TestSymbol> EL;
  EXPECT_EQ(EL.head(), nullptr);
  EXPECT_EQ(EL.size(), 0U);
}

TEST(EmitList, Three) {
  using testing::_;
  using testing::Invoke;

  rld::EmitList<TestSymbol> EL;

  MockTestSymbol S1;
  MockTestSymbol S2;
  MockTestSymbol S3;
  ON_CALL(S1, setNextEmit(_))
      .WillByDefault(Invoke(
          [&S1](TestSymbol *const Next) { return S1.setNextEmitImpl(Next); }));
  ON_CALL(S2, setNextEmit(_))
      .WillByDefault(Invoke(
          [&S2](TestSymbol *const Next) { return S2.setNextEmitImpl(Next); }));
  ON_CALL(S3, setNextEmit(_))
      .WillByDefault(Invoke(
          [&S3](TestSymbol *const Next) { return S3.setNextEmitImpl(Next); }));

  EXPECT_CALL(S1, setNextEmit(&S2));
  EXPECT_CALL(S2, setNextEmit(&S3));

  EL.append(&S1);
  EL.append(&S2);
  EL.append(&S3);
  EL.last();
  EXPECT_EQ(EL.size(), 3U);
  EXPECT_EQ(EL.head(), &S1);
  EXPECT_EQ(S1.NextEmit, &S2);
  EXPECT_EQ(S2.NextEmit, &S3);
  EXPECT_EQ(S3.NextEmit, nullptr);
}
