//===- llvm/unittest/IR/RepoTicketMDTest.cpp - Ticket Metadata tests ------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "llvm/AsmParser/Parser.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/RepoHashCalculator.h"
#include "llvm/IR/RepoTicket.h"
#include "llvm/Support/SourceMgr.h"
#include "gmock/gmock.h"

using namespace llvm;

// Basic ticket metadate test fixture named TicketMDTest.
namespace {
class TicketMDTest : public testing::Test {
protected:
  LLVMContext Ctx;
  IRBuilder<> Builder;

  TicketMDTest() : Builder(Ctx) {}

  std::unique_ptr<Module> parseAssembly(StringRef Assembly) {
    SMDiagnostic Error;
    std::unique_ptr<Module> M = parseAssemblyString(Assembly, Error, Ctx);
    if (!M) {
      std::string errMsg;
      raw_string_ostream os(errMsg);
      Error.print("", os);
      report_fatal_error(os.str());
    } else {
      MD5::MD5Result Hash = {};
      M->setModuleHash(Hash);
    }
    return M;
  }

  const TicketNode *getTicket(const GlobalObject *F) {
    return dyn_cast<TicketNode>(F->getMetadata(LLVMContext::MD_repo_ticket));
  }

  bool isEqualDigest(const GlobalObject *F1, const GlobalObject *F2) {
    return getTicket(F1)->getDigest() == getTicket(F2)->getDigest();
  }
};
} // namespace

// The ticket metadate fixture for single module.
namespace {
class SingleModule : public TicketMDTest {
protected:
  std::unique_ptr<Module> M;
};

} // end anonymous namespace

//
//  IR forming a following call graph for M.
//      foo            bar
//   (return 1)    (return 1)
//
//  Both 'foo' and 'bar' have the same hash value.
// `foo`: Contributions: [ ] : Dependencies: [ ].
// `bar`: Contributions: [ ] : Dependencies: [ ].
//
TEST_F(SingleModule, NoCalleeSame) {
  const char *ModuleString = "define internal i32 @foo() { ret i32 1 }\n"
                             "define internal i32 @bar() { ret i32 1 }\n";
  M = parseAssembly(ModuleString);
  ticketmd::GOInfoMap InfoMap = ticketmd::calculateGONumAndGOIMap(*M);

  const Function *Foo = M->getFunction("foo");
  const ticketmd::GOInfo &FooInfo = InfoMap[Foo];
  const Function *Bar = M->getFunction("bar");
  const ticketmd::GOInfo &BarInfo = InfoMap[Bar];
  EXPECT_EQ(FooInfo.InitialDigest, BarInfo.InitialDigest)
      << "Expected that functions of foo and bar have the same initial hash "
         "value";
  EXPECT_TRUE(FooInfo.Contributions.empty())
      << "Expected that the foo's contribution list is empty.";
  EXPECT_TRUE(FooInfo.Dependencies.empty())
      << "Expected that the foo's dependencies list is empty.";
  EXPECT_TRUE(BarInfo.Contributions.empty())
      << "Expected that the bar's contributions list is empty.";
  EXPECT_TRUE(BarInfo.Dependencies.empty())
      << "Expected that the bar's dependencies list is empty.";
  // Check the GOs' final digest.
  EXPECT_TRUE(ticketmd::generateTicketMDs(*M))
      << "Expected Module M to be changed";
  EXPECT_TRUE(isEqualDigest(Foo, Bar))
      << "Functions of foo and bar should have the same digest";
}

//
//  IR forming a following call graph for M.
//      foo            bar
//   (return 2)    (return 1)
//
//  Both 'foo' and 'bar' have the different digest.
// `foo`: Contributions: [ ] : Dependencies: [ ].
// `bar`: Contributions: [ ] : Dependencies: [ ].
//
TEST_F(SingleModule, NoCalleeDiff) {
  const char *ModuleString = "define internal i32 @foo() { ret i32 2 }\n"
                             "define internal i32 @bar() { ret i32 1 }\n";
  M = parseAssembly(ModuleString);
  ticketmd::generateTicketMDs(*M);
  const Function *Foo = M->getFunction("foo");
  const Function *Bar = M->getFunction("bar");
  EXPECT_FALSE(isEqualDigest(Foo, Bar))
      << "Functions of foo and bar should have the different digest";
}

//
//  IR forming a following call graph for M.
//     foo       bar
//      |         |
//      +--> g <--+
//       (return 1)
//
// Both 'foo' and 'bar' call 'g' function and have the same hash value.
// `foo`: Contributions: [] : Dependencies: [ `g` ].
// `bar`: Contributions: [] : Dependencies: [ `g` ].
//
TEST_F(SingleModule, OneCalleeSameNameSameBody) {
  const char *ModuleString = "define i32 @foo() {\n"
                             "entry:\n"
                             "  %0 = call i32 @g()\n"
                             "  ret i32 %0\n"
                             "}\n"
                             "define i32 @bar() {\n"
                             "entry:\n"
                             "  %0 = call i32 @g()\n"
                             "  ret i32 %0\n"
                             "}\n"
                             "define internal i32 @g() { ret i32 1 }\n";
  M = parseAssembly(ModuleString);
  // Check the GOs' initial digest, contributions and dependencies.
  ticketmd::GOInfoMap InfoMap = ticketmd::calculateGONumAndGOIMap(*M);
  const Function *Foo = M->getFunction("foo");
  const ticketmd::GOInfo &FooInfo = InfoMap[Foo];
  const Function *Bar = M->getFunction("bar");
  const ticketmd::GOInfo &BarInfo = InfoMap[Bar];
  const Function *G = M->getFunction("g");
  EXPECT_EQ(FooInfo.InitialDigest, BarInfo.InitialDigest)
      << "Expected that functions of foo and bar have the same initial hash "
         "value";
  EXPECT_TRUE(FooInfo.Contributions.empty())
      << "Expected that the foo's contributions list is empty.";
  EXPECT_THAT(FooInfo.Dependencies, ::testing::UnorderedElementsAre(G))
      << "Expected foo's Dependencies list is { G }";
  EXPECT_TRUE(BarInfo.Contributions.empty())
      << "Expected that the bar's contributions list is empty.";
  EXPECT_THAT(BarInfo.Dependencies, ::testing::UnorderedElementsAre(G))
      << "Expected bar's Dependencies list is { G }";
  // Check the GOs' final digest.
  EXPECT_TRUE(ticketmd::generateTicketMDs(*M))
      << "Expected Module M to be changed";
  EXPECT_TRUE(isEqualDigest(Foo, Bar))
      << "Functions of foo and bar should have the same digest";
}

//
//  IR forming a following call graph for M0.
//     foo           bar
//      |             |
//      v             v
//     g (1)         p (1)
//
// The 'foo' and 'bar' have the different hash value since they call the
// different name functions ('p' and 'q').
// `foo`: Contributions: [] : Dependencies: [ `g` ].
// `bar`: Contributions: [] : Dependencies: [ `p` ].
//
TEST_F(SingleModule, OneCalleeDiffNameSameBody) {
  const char *ModuleString = "define i32 @foo() {\n"
                             "entry:\n"
                             "  %0 = call i32 @g()\n"
                             "  ret i32 %0\n"
                             "}\n"
                             "define i32 @bar() {\n"
                             "entry:\n"
                             "  %0 = call i32 @p()\n"
                             "  ret i32 %0\n"
                             "}\n"
                             "define internal i32 @g() { ret i32 1 }\n"
                             "define internal i32 @p() { ret i32 1 }\n";
  M = parseAssembly(ModuleString);
  // Check the GOs' initial digest, contributions and dependencies.
  ticketmd::GOInfoMap InfoMap = ticketmd::calculateGONumAndGOIMap(*M);
  const Function *Foo = M->getFunction("foo");
  const ticketmd::GOInfo &FooInfo = InfoMap[Foo];
  const Function *Bar = M->getFunction("bar");
  const ticketmd::GOInfo &BarInfo = InfoMap[Bar];
  const Function *G = M->getFunction("g");
  const Function *P = M->getFunction("p");
  EXPECT_NE(FooInfo.InitialDigest, BarInfo.InitialDigest)
      << "Expected that functions of foo and bar have the different initial "
         "hash value";
  EXPECT_TRUE(FooInfo.Contributions.empty())
      << "Expected that the foo's contributions list is empty.";
  EXPECT_THAT(FooInfo.Dependencies, ::testing::UnorderedElementsAre(G))
      << "Expected foo's Dependencies list is { G }";
  EXPECT_TRUE(BarInfo.Contributions.empty())
      << "Expected that the bar's contributions list is empty.";
  EXPECT_THAT(BarInfo.Dependencies, ::testing::UnorderedElementsAre(P))
      << "Expected bar's Dependencies list is { P }";
  // Check the GOs' final digest.
  EXPECT_TRUE(ticketmd::generateTicketMDs(*M))
      << "Expected Module M to be changed";
  EXPECT_TRUE(isEqualDigest(G, P))
      << "Functions of p and q should have the same digest";
  EXPECT_FALSE(isEqualDigest(Foo, Bar))
      << "Functions of foo and bar should have the different digest";
}

//
//  IR forming a following call graph for M.
//     foo           bar
//      |             |
//      v             v
//     g (1)         p (2)
//
// The 'foo' and 'bar' have the different hash value since they call the
// different functions ('p' and 'q').
//
TEST_F(SingleModule, OneCalleeDiffNameDiffBody) {
  const char *ModuleString = "define i32 @foo() {\n"
                             "entry:\n"
                             "  %0 = call i32 @g()\n"
                             "  ret i32 %0\n"
                             "}\n"
                             "define i32 @bar() {\n"
                             "entry:\n"
                             "  %0 = call i32 @p()\n"
                             "  ret i32 %0\n"
                             "}\n"
                             "define internal i32 @g() { ret i32 1 }\n"
                             "define internal i32 @p() { ret i32 2 }\n";
  M = parseAssembly(ModuleString);
  ticketmd::generateTicketMDs(*M);
  const Function *G = M->getFunction("g");
  const Function *P = M->getFunction("p");
  EXPECT_FALSE(isEqualDigest(G, P))
      << "Functions of p and q should have the different digest";
  const Function *Foo = M->getFunction("foo");
  const Function *Bar = M->getFunction("bar");
  EXPECT_FALSE(isEqualDigest(Foo, Bar))
      << "Functions of foo and bar should have the different digest";
}

//
//  IR forming a following call graph.
//       foo               bar
//        |                 |
//        v                 v
//        g                 p
//  (declare only)   (declare only)
//  (Indirect call)  (Indirect call)
//
TEST_F(SingleModule, IndirectCall) {
  const char *ModuleString =
      "define i32 @foo() {\n"
      "entry:\n"
      "  %call = call i32 bitcast (i32 (...)* @g to i32 ()*)()\n"
      "  ret i32 %call\n"
      "}\n"
      "define i32 @bar() {\n"
      "entry:\n"
      "  %call = call i32 bitcast (i32 (...)* @p to i32 ()*)()\n"
      "  ret i32 %call\n"
      "}\n"
      "declare i32 @g(...)\n"
      "declare i32 @p(...)\n";
  M = parseAssembly(ModuleString);
  EXPECT_TRUE(ticketmd::generateTicketMDs(*M))
      << "Expected Module M to be changed";
  const Function *Foo = M->getFunction("foo");
  const Function *Bar = M->getFunction("bar");
  EXPECT_FALSE(isEqualDigest(Foo, Bar))
      << "Functions of foo and bar should have the different digest";
}

//
//  IR forming a following call graph.
//       foo               bar
//        |                 |
//        v                 v
//        g                 p
//  (declare only)   (declare only)
//  (direct call)    (direct call)
//
TEST_F(SingleModule, DirectCall) {
  const char *ModuleString = "define i32 @foo() {\n"
                             "entry:\n"
                             "  %call = call i32 (...) @g()\n"
                             "  ret i32 %call\n"
                             "}\n"
                             "define i32 @bar() {\n"
                             "entry:\n"
                             "  %call = call i32 (...) @p()\n"
                             "  ret i32 %call\n"
                             "}\n"
                             "declare i32 @g(...)\n"
                             "declare i32 @p(...)\n";
  M = parseAssembly(ModuleString);
  ticketmd::generateTicketMDs(*M);
  const Function *Foo = M->getFunction("foo");
  const Function *Bar = M->getFunction("bar");
  EXPECT_FALSE(isEqualDigest(Foo, Bar))
      << "Functions of foo and bar should have the different digest";
}

//
//  foo calls bar and bar calls foo.
//     foo   ---->  bar
//      |           |
//      <-----------+
//
//  The 'foo' and 'bar' have the different hash value since they call the
//  different name functions ('bar' and 'foo').
//  `foo`: Contributions: [] : Dependencies: [ `bar` ].
//  `bar`: Contributions: [] : Dependencies: [ `foo` ].
//
TEST_F(SingleModule, CallEachOther) {
  const char *ModuleString = "define void @foo() {\n"
                             "entry:\n"
                             "call void @bar()\n"
                             "ret void\n"
                             "}\n"
                             "define void @bar() {\n"
                             "entry:\n"
                             "call void @foo()\n"
                             "ret void\n"
                             "}\n";
  M = parseAssembly(ModuleString);
  // Check the GOs' initial digest, contributions and dependencies.
  ticketmd::GOInfoMap InfoMap = ticketmd::calculateGONumAndGOIMap(*M);
  const Function *Foo = M->getFunction("foo");
  const ticketmd::GOInfo &FooInfo = InfoMap[Foo];
  const Function *Bar = M->getFunction("bar");
  const ticketmd::GOInfo &BarInfo = InfoMap[Bar];
  EXPECT_NE(FooInfo.InitialDigest, BarInfo.InitialDigest)
      << "Expected that functions of foo and bar have the different initial "
         "hash value";
  EXPECT_TRUE(FooInfo.Contributions.empty())
      << "Expected that the foo's contributions list is empty.";
  EXPECT_THAT(FooInfo.Dependencies, ::testing::ElementsAre(Bar))
      << "Expected foo's Dependencies list is {bar}";
  EXPECT_TRUE(BarInfo.Contributions.empty())
      << "Expected that the bar's contributions list is empty.";
  EXPECT_THAT(BarInfo.Dependencies, ::testing::ElementsAre(Foo))
      << "Expected bar's Dependencies list is {foo}";
  // Check the GOs' final digest.
  EXPECT_TRUE(ticketmd::generateTicketMDs(*M))
      << "Expected Module M to be changed";
  EXPECT_FALSE(isEqualDigest(Foo, Bar))
      << "Expected that functions of foo and bar have the different final hash "
         "value";
}

//
//    Foo         Bar <-------+
//     |           |          |
//     +---> P <-- +          |
//           |                |
//           +----------------+
//
//  `foo`: Contributions: [] : Dependencies: [ `p` ].
//  `bar`: Contributions: [] : Dependencies: [ `p` ].
//  `p`  : Contributions: [] : Dependencies: [ `bar` ].
//
TEST_F(SingleModule, OneCalleeLoop) {
  const char *ModuleString = "define i32 @foo() {\n"
                             "entry:\n"
                             "  %0 = call i32 @p()\n"
                             "  ret i32 %0\n"
                             "}\n"
                             "define i32 @bar() {\n"
                             "entry:\n"
                             "  %0 = call i32 @p()\n"
                             "  ret i32 %0\n"
                             "}\n"
                             "define internal i32 @p() {\n"
                             "entry:\n"
                             "  %0 = call i32 @bar()\n"
                             "  ret i32 %0\n"
                             "}\n";
  M = parseAssembly(ModuleString);
  // Check the GOs' initial digest, contributions and dependencies.
  ticketmd::GOInfoMap InfoMap = ticketmd::calculateGONumAndGOIMap(*M);
  const Function *Foo = M->getFunction("foo");
  const ticketmd::GOInfo &FooInfo = InfoMap[Foo];
  const Function *Bar = M->getFunction("bar");
  const ticketmd::GOInfo &BarInfo = InfoMap[Bar];
  const Function *P = M->getFunction("p");
  const ticketmd::GOInfo &PInfo = InfoMap[P];
  EXPECT_EQ(FooInfo.InitialDigest, BarInfo.InitialDigest)
      << "Expected that functions of foo and bar have the same initial hash "
         "value";
  EXPECT_TRUE(FooInfo.Contributions.empty())
      << "Expected that the foo's contributions list is empty.";
  EXPECT_THAT(FooInfo.Dependencies, ::testing::ElementsAre(P))
      << "Expected foo's Dependencies list is {P}";
  EXPECT_TRUE(BarInfo.Contributions.empty())
      << "Expected that the bar's contributions list is empty.";
  EXPECT_THAT(BarInfo.Dependencies, ::testing::ElementsAre(P))
      << "Expected bar's Dependencies list is {P}";
  EXPECT_TRUE(PInfo.Contributions.empty())
      << "Expected that the p's contributions list is empty.";
  EXPECT_THAT(PInfo.Dependencies, ::testing::ElementsAre(Bar))
      << "Expected p's Dependencies list is {bar}";
  // Check the GOs' final digest.
  EXPECT_TRUE(ticketmd::generateTicketMDs(*M))
      << "Expected Module M to be changed";
  EXPECT_FALSE(isEqualDigest(Foo, Bar))
      << "Expected that functions of foo and bar have the different final "
         "hash value";
}

//
//  Two levels calls.
//     foo   bar
//      |  \/  |
//      |  /\  |
//      v v  v v
//       p    q
//       |
//       v
//       z
//
//  `foo`: Contributions: [] : Dependencies: [ `p`,  `q`].
//  `bar`: Contributions: [] : Dependencies: [ `p`,  `q` ].
//  `p`  : Contributions: [] : Dependencies: [ `z` ].
//  `q`  : Contributions: [] : Dependencies: [ ].
//  `z`  : Contributions: [] : Dependencies: [ ].
//
TEST_F(SingleModule, TwolevelsCall) {
  const char *ModuleString = "define i32 @foo() {\n"
                             "entry:\n"
                             " %call = call i32 @p()\n"
                             " %call1 = call i32 @q()\n"
                             " %add = add nsw i32 %call, %call1\n"
                             " ret i32 %add\n"
                             "}\n"
                             "define i32 @bar() {\n"
                             "entry:\n"
                             " %call = call i32 @p()\n"
                             " %call1 = call i32 @q()\n"
                             " %add = add nsw i32 %call, %call1\n"
                             " ret i32 %add\n"
                             "}\n"
                             "define i32 @z() {\n"
                             "entry:\n"
                             " ret i32 1\n"
                             "}\n"
                             "define i32 @p() {\n"
                             "entry:\n"
                             " %call = call i32 @z()\n"
                             " %add = add nsw i32 %call, 1\n"
                             "ret i32 %add\n"
                             "}\n"
                             "define i32 @q() {\n"
                             "entry:\n"
                             " ret i32 1\n"
                             "}\n";
  M = parseAssembly(ModuleString);
  // Check the GOs' initial digest, contributions and dependencies.
  ticketmd::GOInfoMap InfoMap = ticketmd::calculateGONumAndGOIMap(*M);
  const Function *Foo = M->getFunction("foo");
  const ticketmd::GOInfo &FooInfo = InfoMap[Foo];
  const Function *Bar = M->getFunction("bar");
  const ticketmd::GOInfo &BarInfo = InfoMap[Bar];
  const Function *P = M->getFunction("p");
  const ticketmd::GOInfo &PInfo = InfoMap[P];
  const Function *Q = M->getFunction("q");
  const ticketmd::GOInfo &QInfo = InfoMap[Q];
  const Function *Z = M->getFunction("z");
  const ticketmd::GOInfo &ZInfo = InfoMap[Z];

  EXPECT_EQ(ZInfo.InitialDigest, QInfo.InitialDigest)
      << "Expected that functions of z and q have the same initial "
         "hash value";
  EXPECT_EQ(FooInfo.InitialDigest, BarInfo.InitialDigest)
      << "Expected that functions of foo and bar have the same initial "
         "hash value";

  EXPECT_TRUE(ZInfo.Contributions.empty())
      << "Expected that the z's Contributions list is empty.";
  EXPECT_TRUE(ZInfo.Dependencies.empty())
      << "Expected that the z's Dependencies list is empty.";
  EXPECT_TRUE(QInfo.Contributions.empty())
      << "Expected that the q's Contributions list is empty.";
  EXPECT_TRUE(QInfo.Dependencies.empty())
      << "Expected that the q's Dependencies list is empty.";
  EXPECT_TRUE(PInfo.Contributions.empty())
      << "Expected that the p's Contributions list is empty.";
  EXPECT_THAT(PInfo.Dependencies, ::testing::ElementsAre(Z))
      << "Expected that the p's Dependencies list is {Z}";
  EXPECT_TRUE(FooInfo.Contributions.empty())
      << "Expected that the foo's Contributions list is empty.";
  EXPECT_THAT(FooInfo.Dependencies, ::testing::UnorderedElementsAre(P, Q))
      << "Expected that the foo's Dependencies list is {P, Q}";
  EXPECT_TRUE(BarInfo.Contributions.empty())
      << "Expected that the bar's Contributions list is empty.";
  EXPECT_THAT(BarInfo.Dependencies, ::testing::UnorderedElementsAre(P, Q))
      << "Expected that the bar's Dependencies list is {P, Q}";

  EXPECT_TRUE(ticketmd::generateTicketMDs(*M))
      << "Expected Module M to be changed";
  EXPECT_TRUE(isEqualDigest(Foo, Bar)) << "Expected that functions of foo and "
                                          "bar have the same final hash value";
}

//
//  Z has a single contribution.
//        test
//         /\
//        /  \
//       v    \
//     setto   \
//        \    /
//         \  /
//         v  v
//           Z
//
//  `Z`     : Contributions: [ `test`] : Dependencies: [ ].
//  `test`  : Contributions: [ ]       : Dependencies: [ `Z`,  `setto` ].
//  `setto` : Contributions: [ ]       : Dependencies: [ ].
//
TEST_F(SingleModule, SingleContribution) {
  const char *ModuleString = "@Z = global i32 1\n"
                             "define void @test() {\n"
                             "    call void @setto( i32* @Z, i32 3 )\n"
                             "    ret void\n"
                             "}\n"
                             "define void @setto(i32* %P, i32 %V) {\n"
                             "    store i32 %V, i32* %P\n"
                             "    ret void\n"
                             "}\n";
  M = parseAssembly(ModuleString);
  // Check the GOs' initial digest, contributions and dependencies.
  ticketmd::GOInfoMap InfoMap = ticketmd::calculateGONumAndGOIMap(*M);
  const GlobalVariable *Z = M->getGlobalVariable("Z");
  const ticketmd::GOInfo &ZInfo = InfoMap[Z];
  const Function *Test = M->getFunction("test");
  const ticketmd::GOInfo &TestInfo = InfoMap[Test];
  const Function *Setto = M->getFunction("setto");
  const ticketmd::GOInfo &SettoInfo = InfoMap[Setto];

  EXPECT_THAT(ZInfo.Contributions, ::testing::UnorderedElementsAre(Test))
      << "Expected that the Z's Contributions list is {Test}";
  EXPECT_TRUE(ZInfo.Dependencies.empty())
      << "Expected that Z's Dependencies list is empty";
  EXPECT_TRUE(TestInfo.Contributions.empty())
      << "Expected that the test's Contributions list is empty.";
  EXPECT_THAT(TestInfo.Dependencies, ::testing::UnorderedElementsAre(Z, Setto))
      << "Expected that the test's Dependencies list is {Z, setto}";
  EXPECT_TRUE(SettoInfo.Contributions.empty())
      << "Expected that the setto's Contributions list is empty.";
  EXPECT_TRUE(SettoInfo.Dependencies.empty())
      << "Expected that the setto's Dependencies list is empty.";
}

//
//  Z has multiple contributions.
//        test   test1    test2
//         /\     /\       /\
//        /  \   /  \     /  \
//       v    \ /    \   /    \
//     setto <-\------\-/      \
//        \    /       \        \
//         \  /        /        /
//         v v <------/        /
//           Z <--------------/
//
//  `Z`     : Contributions: [ `test`, `test1`, `test2`] : Dependencies: [ ].
//  `test`  : Contributions: [ ] : Dependencies: [ `Z`,  `setto` ].
//  `test1` : Contributions: [ ] : Dependencies: [ `Z`,  `setto` ].
//  `test2` : Contributions: [ ] : Dependencies: [ `Z`,  `setto` ].
//  `setto` : Contributions: [ ] : Dependencies: [ ].
//
TEST_F(SingleModule, MultipleContribution) {
  const char *ModuleString = "@Z = global i32 1\n"
                             "define void @test() {\n"
                             "    call void @setto( i32* @Z, i32 1 )\n"
                             "    ret void\n"
                             "}\n"
                             "define void @test1() {\n"
                             "    call void @setto( i32* @Z, i32 2 )\n"
                             "    ret void\n"
                             "}\n"
                             "define void @test2() {\n"
                             "    call void @setto( i32* @Z, i32 3 )\n"
                             "    ret void\n"
                             "}\n"
                             "define void @setto(i32* %P, i32 %V) {\n"
                             "    store i32 %V, i32* %P\n"
                             "    ret void\n"
                             "}\n";
  M = parseAssembly(ModuleString);
  // Check the GOs' initial digest, contributions and dependencies.
  ticketmd::GOInfoMap InfoMap = ticketmd::calculateGONumAndGOIMap(*M);
  const GlobalVariable *Z = M->getGlobalVariable("Z");
  const ticketmd::GOInfo &ZInfo = InfoMap[Z];
  const Function *Test = M->getFunction("test");
  const Function *Test1 = M->getFunction("test1");
  const Function *Test2 = M->getFunction("test2");

  EXPECT_THAT(ZInfo.Contributions,
              ::testing::UnorderedElementsAre(Test, Test1, Test2))
      << "Expected that the Z's Contributions list is {Test, Test1, Test2}";
  EXPECT_TRUE(ZInfo.Dependencies.empty())
      << "Expected that Z's Dependencies list is empty";
}

// The ticket metadate fixture for double modules.
namespace {
class DoubleModule : public TicketMDTest {
protected:
  std::unique_ptr<Module> M0;
  std::unique_ptr<Module> M1;
};
} // namespace

//
//  IR forming a following call graph for M0 and M1.
// Module: M0     Module: M1
//     foo		     bar
//      |             |
//      v             v
//     g (1)		 g (2)
//
// The function 'foo' hashes in Module M0 and M1 are different hash value since
// the funtion 'g' hashes are in Module M0 and M1 different.
//
TEST_F(DoubleModule, OneCalleeSameNameDiffBody) {
  const char *Module0String = "define i32 @foo() {\n"
                              "entry:\n"
                              "  %0 = call i32 @g()\n"
                              "  ret i32 %0\n"
                              "}\n"
                              "define internal i32 @g() { ret i32 1 }\n";
  const char *Module1String = "define i32 @bar() {\n"
                              "entry:\n"
                              "  %0 = call i32 @g()\n"
                              "  ret i32 %0\n"
                              "}\n"
                              "define internal i32 @g() { ret i32 2 }\n";
  M0 = parseAssembly(Module0String);
  M1 = parseAssembly(Module1String);
  ticketmd::generateTicketMDs(*M0);
  ticketmd::generateTicketMDs(*M1);
  const Function *Foo = M0->getFunction("foo");
  const Function *Bar = M1->getFunction("bar");
  EXPECT_FALSE(isEqualDigest(Foo, Bar))
      << "Functions of foo and bar should have the different digest";
}

//  Module M0:      Module M1:
//    Foo             Bar <----+
//     |               |       |
//     v               v       |
//     g               g-------+
//
TEST_F(DoubleModule, OneCalleeLoop) {
  const char *Module0String = "define i32 @foo() {\n"
                              "entry:\n"
                              "  %0 = call i32 @g()\n"
                              "  ret i32 %0\n"
                              "}\n"
                              "define internal i32 @g() { ret i32 1 }\n";
  const char *Module1String = "define i32 @bar() {\n"
                              "entry:\n"
                              "  %0 = call i32 @g()\n"
                              "  ret i32 %0\n"
                              "}\n"
                              "define internal i32 @g() {\n"
                              "entry:\n"
                              "  %0 = call i32 @bar()\n"
                              "  ret i32 %0\n"
                              "}\n";
  M0 = parseAssembly(Module0String);
  M1 = parseAssembly(Module1String);
  ticketmd::generateTicketMDs(*M0);
  ticketmd::generateTicketMDs(*M1);
  const Function *Foo = M0->getFunction("foo");
  const Function *Bar = M1->getFunction("bar");
  EXPECT_FALSE(isEqualDigest(Foo, Bar))
      << "Functions of foo and bar should have the different digest";
}

//
//  IR forming a following call graph for M0 and M1.
// Module: M0     Module: M1
//     foo		     bar
//      |             |
//      v             v
//     g (1)		 g (1)
//
// Both hashes of 'foo' and 'g' in Module M0 are generated by the frontend.
// Both hashes of 'bar' and 'g' in Module M1 are generated by the backend.
// The 'foo' and 'bar' have the same hash value.
//
TEST_F(DoubleModule, FrontendAndBackendHashGeneration) {
  const char *Module0String = "define i32 @foo() {\n"
                              "entry:\n"
                              "  %0 = call i32 @g()\n"
                              "  ret i32 %0\n"
                              "}\n"
                              "define internal i32 @g() { ret i32 1 }\n";
  const char *Module1String = "define i32 @bar() {\n"
                              "entry:\n"
                              "  %0 = call i32 @g()\n"
                              "  ret i32 %0\n"
                              "}\n"
                              "define internal i32 @g() { ret i32 1 }\n";

  M0 = parseAssembly(Module0String);
  ticketmd::generateTicketMDs(*M0);
  const Function *Foo = M0->getFunction("foo");

  M1 = parseAssembly(Module1String);
  auto BarDigest = ticketmd::calculateDigest(M1->getFunction("bar"));

  EXPECT_EQ(getTicket(Foo)->getDigest(), BarDigest)
      << "Functions of foo and bar should have the same digest";
}

//
//  IR forming a following call graph for M0 and M1.
// Module: M0     Module: M1
//     foo		     bar
//      |             |
//      v             v
//     g (1)		 g (1)
//
// Both hashes of 'foo' and 'g'  in Module M0 are generated by the frontend.
// In Module M1, the function 'g' hash is generated by the frontend and 'bar'
// hash is generated by the backend. The 'foo' and 'bar' have the different hash
// value.
//
TEST_F(DoubleModule, MixedFrontendAndBackendHashGeneration) {
  const char *Module0String = "define i32 @foo() {\n"
                              "entry:\n"
                              "  %0 = call i32 @g()\n"
                              "  ret i32 %0\n"
                              "}\n"
                              "define internal i32 @g() { ret i32 1 }\n";
  const char *Module1String = "define i32 @bar() {\n"
                              "entry:\n"
                              "  %0 = call i32 @g()\n"
                              "  ret i32 %0\n"
                              "}\n"
                              "define internal i32 @g() { ret i32 1 }\n";

  M0 = parseAssembly(Module0String);
  ticketmd::generateTicketMDs(*M0);
  const Function *Foo = M0->getFunction("foo");

  M1 = parseAssembly(Module1String);
  Function *G = M1->getFunction("g");
  ticketmd::set(G, ticketmd::calculateDigest(G));
  const Function *Bar = M1->getFunction("bar");
  auto BarDigest = ticketmd::calculateDigest(Bar);

  EXPECT_NE(getTicket(Foo)->getDigest(), BarDigest)
      << "Functions of foo and bar should have the different digest";
}

//
// The following four tests are targeted on checking that the function
// definition order should not affect the function hash. Modules M0 and M1 have
// the same call graph but have different function definition order.
//
//  IR forming a following call graph for M0 and M1.
//      c
//     / \
//    v   v
//    a   b
// Functions 'a' 'b' and 'c' should have the same hashes in Module M0 and M1.
//
TEST_F(DoubleModule, Simple) {
  const char *Module0String = "define i32 @A(){\n"
                              "entry:\n"
                              "  ret i32 1\n"
                              "}\n"
                              "define i32 @B(){\n"
                              "entry:\n"
                              "  ret i32 2\n"
                              "}\n"
                              "define i32 @C() {\n"
                              "entry:\n"
                              "  %call = call i32 @A()\n"
                              "  %call1 = call i32 @B()\n"
                              "  %add = add nsw i32 %call, %call1\n"
                              "  ret i32 %add\n"
                              "}\n";
  const char *Module1String = "define i32 @C() {\n"
                              "entry:\n"
                              "  %call = call i32 @A()\n"
                              "  %call1 = call i32 @B()\n"
                              "  %add = add nsw i32 %call, %call1\n"
                              "  ret i32 %add\n"
                              "}\n"
                              "define i32 @B(){\n"
                              "entry:\n"
                              "  ret i32 2\n"
                              "}\n"
                              "define i32 @A(){\n"
                              "entry:\n"
                              "  ret i32 1\n"
                              "}\n";

  M0 = parseAssembly(Module0String);
  M1 = parseAssembly(Module1String);
  ticketmd::generateTicketMDs(*M0);
  ticketmd::generateTicketMDs(*M1);
  const Function *M0A = M0->getFunction("A");
  const Function *M1A = M1->getFunction("A");
  EXPECT_TRUE(isEqualDigest(M0A, M1A))
      << "Function A should have the same digest in M0 and M1";
  const Function *M0B = M0->getFunction("B");
  const Function *M1B = M1->getFunction("B");
  EXPECT_TRUE(isEqualDigest(M0B, M1B))
      << "Functions of M0B and M1B should have the same digest";
  const Function *M0C = M0->getFunction("C");
  const Function *M1C = M1->getFunction("C");
  EXPECT_TRUE(isEqualDigest(M0C, M1C))
      << "Functions of M0C and M1C should have the same digest";
}

//  IR forming a following call graph for M0 and M1.
//     A
//     |
//     v
//     B <----+
//     |      |
//     + -----+
//
TEST_F(DoubleModule, SelfLoop) {
  const char *Module0String = "define void @A(){\n"
                              "entry:\n"
                              "  call void @B()\n"
                              "  ret void\n"
                              "}\n"
                              "define void @B(){\n"
                              "entry:\n"
                              "  call void @B()\n"
                              "  ret void\n"
                              "}\n";
  const char *Module1String = "define void @B(){\n"
                              "entry:\n"
                              "  call void @B()\n"
                              "  ret void\n"
                              "}\n"
                              "define void @A(){\n"
                              "entry:\n"
                              "  call void @B()\n"
                              "  ret void\n"
                              "}\n";
  M0 = parseAssembly(Module0String);
  M1 = parseAssembly(Module1String);
  ticketmd::generateTicketMDs(*M0);
  ticketmd::generateTicketMDs(*M1);
  const Function *M0A = M0->getFunction("A");
  const Function *M1A = M1->getFunction("A");
  EXPECT_TRUE(isEqualDigest(M0A, M1A))
      << "Function A should have the same digest in M0 and M1";
  const Function *M0B = M0->getFunction("B");
  const Function *M1B = M1->getFunction("B");
  EXPECT_TRUE(isEqualDigest(M0B, M1B))
      << "Functions of M0B and M1B should have the same digest";
}

//  IR forming a following call graph for M0 and M1.
//
//     A <----+
//     |      |
//     v      |
//     B ----->
//
TEST_F(DoubleModule, TinyLoop) {
  const char *Module0String = "define void @A(){\n"
                              "entry:\n"
                              "  call void @B()\n"
                              "  ret void\n"
                              "}\n"
                              "define void @B(){\n"
                              "entry:\n"
                              "  call void @A()\n"
                              "  ret void\n"
                              "}\n";
  const char *Module1String = "define void @B(){\n"
                              "entry:\n"
                              "  call void @A()\n"
                              "  ret void\n"
                              "}\n"
                              "define void @A(){\n"
                              "entry:\n"
                              "  call void @B()\n"
                              "  ret void\n"
                              "}\n";
  M0 = parseAssembly(Module0String);
  M1 = parseAssembly(Module1String);
  ticketmd::generateTicketMDs(*M0);
  ticketmd::generateTicketMDs(*M1);
  const Function *M0A = M0->getFunction("A");
  const Function *M1A = M1->getFunction("A");
  EXPECT_TRUE(isEqualDigest(M0A, M1A))
      << "Function A should have the same digest in M0 and M1";
  const Function *M0B = M0->getFunction("B");
  const Function *M1B = M1->getFunction("B");
  EXPECT_TRUE(isEqualDigest(M0B, M1B))
      << "Functions of M0B and M1B should have the same digest";
}

//  IR forming a following call graph for M0 and M1.
//     C
//     |
//     v
//     A <----+
//     |      |
//     v      |
//     B ----->
//
TEST_F(DoubleModule, TinyLoop2) {
  const char *Module0String = "define void @A(){\n"
                              "entry:\n"
                              "  call void @B()\n"
                              "  ret void\n"
                              "}\n"
                              "define void @B(){\n"
                              "entry:\n"
                              "  call void @A()\n"
                              "  ret void\n"
                              "}\n"
                              "define void @C(){\n"
                              "entry:\n"
                              "  call void @A()\n"
                              "  ret void\n"
                              "}\n";
  const char *Module1String = "define void @C(){\n"
                              "entry:\n"
                              "  call void @A()\n"
                              "  ret void\n"
                              "}\n"
                              "define void @B(){\n"
                              "entry:\n"
                              "  call void @A()\n"
                              "  ret void\n"
                              "}\n"
                              "define void @A(){\n"
                              "entry:\n"
                              "  call void @B()\n"
                              "  ret void\n"
                              "}\n";
  M0 = parseAssembly(Module0String);
  M1 = parseAssembly(Module1String);
  ticketmd::generateTicketMDs(*M0);
  ticketmd::generateTicketMDs(*M1);
  const Function *M0A = M0->getFunction("A");
  const Function *M1A = M1->getFunction("A");
  EXPECT_TRUE(isEqualDigest(M0A, M1A))
      << "Function A should have the same digest in M0 and M1";
  const Function *M0B = M0->getFunction("B");
  const Function *M1B = M1->getFunction("B");
  EXPECT_TRUE(isEqualDigest(M0B, M1B))
      << "Functions of M0B and M1B should have the same digest";
  const Function *M0C = M0->getFunction("C");
  const Function *M1C = M1->getFunction("C");
  EXPECT_TRUE(isEqualDigest(M0C, M1C))
      << "Functions of M0C and M1C should have the same digest";
}

//  IR forming a following call graph for M0 and M1.
//                G
//                |
//     +--------------------+
//     |                    |
//     v                    v
//     C <----+             F <----+
//     |      |             |      |
//     v      |             v      |
//     B      |             E      |
//     |      |             |      |
//     v      |             v      |
//     A ----->             D ----->
//
TEST_F(DoubleModule, TwoLoop) {
  const char *Module0String = "define void @G(){\n"
                              "entry:\n"
                              "  call void @C()\n"
                              "  call void @F()\n"
                              "  ret void\n"
                              "}\n"
                              "define void @C(){\n"
                              "entry:\n"
                              "  call void @B()\n"
                              "  ret void\n"
                              "}\n"
                              "define void @B(){\n"
                              "entry:\n"
                              "  call void @A()\n"
                              "  ret void\n"
                              "}\n"
                              "define void @A(){\n"
                              "entry:\n"
                              "  call void @C()\n"
                              "  ret void\n"
                              "}\n"
                              "define void @F(){\n"
                              "entry:\n"
                              "  call void @E()\n"
                              "  ret void\n"
                              "}\n"
                              "define void @E(){\n"
                              "entry:\n"
                              "  call void @D()\n"
                              "  ret void\n"
                              "}\n"
                              "define void @D(){\n"
                              "entry:\n"
                              "  call void @F()\n"
                              "  ret void\n"
                              "}\n";
  const char *Module1String = "define void @D(){\n"
                              "entry:\n"
                              "  call void @F()\n"
                              "  ret void\n"
                              "}\n"
                              "define void @E(){\n"
                              "entry:\n"
                              "  call void @D()\n"
                              "  ret void\n"
                              "}\n"
                              "define void @F(){\n"
                              "entry:\n"
                              "  call void @E()\n"
                              "  ret void\n"
                              "}\n"
                              "define void @A(){\n"
                              "entry:\n"
                              "  call void @C()\n"
                              "  ret void\n"
                              "}\n"
                              "define void @B(){\n"
                              "entry:\n"
                              "  call void @A()\n"
                              "  ret void\n"
                              "}\n"
                              "define void @C(){\n"
                              "entry:\n"
                              "  call void @B()\n"
                              "  ret void\n"
                              "}\n"
                              "define void @G(){\n"
                              "entry:\n"
                              "  call void @C()\n"
                              "  call void @F()\n"
                              "  ret void\n"
                              "}\n";
  M0 = parseAssembly(Module0String);
  M1 = parseAssembly(Module1String);
  ticketmd::generateTicketMDs(*M0);
  ticketmd::generateTicketMDs(*M1);
  const Function *M0A = M0->getFunction("A");
  const Function *M1A = M1->getFunction("A");
  EXPECT_TRUE(isEqualDigest(M0A, M1A))
      << "Function A should have the same digest in M0 and M1";
  const Function *M0B = M0->getFunction("B");
  const Function *M1B = M1->getFunction("B");
  EXPECT_TRUE(isEqualDigest(M0B, M1B))
      << "Functions of M0B and M1B should have the same digest";
  const Function *M0C = M0->getFunction("C");
  const Function *M1C = M1->getFunction("C");
  EXPECT_TRUE(isEqualDigest(M0C, M1C))
      << "Functions of M0C and M1C should have the same digest";
  const Function *M0D = M0->getFunction("D");
  const Function *M1D = M1->getFunction("D");
  EXPECT_TRUE(isEqualDigest(M0D, M1D))
      << "Functions of M0D and M1D should have the same digest";
  const Function *M0E = M0->getFunction("E");
  const Function *M1E = M1->getFunction("E");
  EXPECT_TRUE(isEqualDigest(M0E, M1E))
      << "Functions of M0E and M1E should have the same digest";
  const Function *M0F = M0->getFunction("F");
  const Function *M1F = M1->getFunction("F");
  EXPECT_TRUE(isEqualDigest(M0F, M1F))
      << "Functions of M0F and M1F should have the same digest";
  const Function *M0G = M0->getFunction("G");
  const Function *M1G = M1->getFunction("G");
  EXPECT_TRUE(isEqualDigest(M0G, M1G))
      << "Functions of M0G and M1G should have the same digest";
}

//  IR forming a following call graph for M0 and M1.
//                A
//                |
//     +--------------------+
//     |                    |
//     v                    v
//     B <----+             D------+
//     |      |             |      |
//     v      |             v      v
//     C ----->             E      F
//
TEST_F(DoubleModule, Hybrid) {
  const char *Module0String = "define void @A(){\n"
                              "entry:\n"
                              "  call void @B()\n"
                              "  call void @D()\n"
                              "  ret void\n"
                              "}\n"
                              "define void @B(){\n"
                              "entry:\n"
                              "  call void @C()\n"
                              "  ret void\n"
                              "}\n"
                              "define void @C(){\n"
                              "entry:\n"
                              "  call void @B()\n"
                              "  ret void\n"
                              "}\n"
                              "define void @D(){\n"
                              "entry:\n"
                              "  call i32 @E()\n"
                              "  call i32 @F()\n"
                              "  ret void\n"
                              "}\n"
                              "define i32 @E(){\n"
                              "entry:\n"
                              "  ret i32 1\n"
                              "}\n"
                              "define i32 @F(){\n"
                              "entry:\n"
                              "  ret i32 2\n"
                              "}\n";
  const char *Module1String = "define i32 @F(){\n"
                              "entry:\n"
                              "  ret i32 2\n"
                              "}\n"
                              "define i32 @E(){\n"
                              "entry:\n"
                              "  ret i32 1\n"
                              "}\n"
                              "define void @D(){\n"
                              "entry:\n"
                              "  call i32 @E()\n"
                              "  call i32 @F()\n"
                              "  ret void\n"
                              "}\n"
                              "define void @C(){\n"
                              "entry:\n"
                              "  call void @B()\n"
                              "  ret void\n"
                              "}\n"
                              "define void @B(){\n"
                              "entry:\n"
                              "  call void @C()\n"
                              "  ret void\n"
                              "}\n"
                              "define void @A(){\n"
                              "entry:\n"
                              "  call void @B()\n"
                              "  call void @D()\n"
                              "  ret void\n"
                              "}\n";
  M0 = parseAssembly(Module0String);
  M1 = parseAssembly(Module1String);
  ticketmd::generateTicketMDs(*M0);
  ticketmd::generateTicketMDs(*M1);
  const Function *M0A = M0->getFunction("A");
  const Function *M1A = M1->getFunction("A");
  EXPECT_TRUE(isEqualDigest(M0A, M1A))
      << "Function A should have the same digest in M0 and M1";
  const Function *M0B = M0->getFunction("B");
  const Function *M1B = M1->getFunction("B");
  EXPECT_TRUE(isEqualDigest(M0B, M1B))
      << "Functions of M0B and M1B should have the same digest";
  const Function *M0C = M0->getFunction("C");
  const Function *M1C = M1->getFunction("C");
  EXPECT_TRUE(isEqualDigest(M0C, M1C))
      << "Functions of M0C and M1C should have the same digest";
  const Function *M0D = M0->getFunction("D");
  const Function *M1D = M1->getFunction("D");
  EXPECT_TRUE(isEqualDigest(M0D, M1D))
      << "Functions of M0D and M1D should have the same digest";
  const Function *M0E = M0->getFunction("E");
  const Function *M1E = M1->getFunction("E");
  EXPECT_TRUE(isEqualDigest(M0E, M1E))
      << "Functions of M0E and M1E should have the same digest";
  const Function *M0F = M0->getFunction("F");
  const Function *M1F = M1->getFunction("F");
  EXPECT_TRUE(isEqualDigest(M0F, M1F))
      << "Functions of M0F and M1F should have the same digest";
}
