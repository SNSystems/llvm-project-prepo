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

  /// Get the GO's information if it is in the information map, otherwise return
  /// an assert failure.
  template <typename GlobalType>
  ticketmd::GOInfo getGOInfo(const GlobalType *G) {
    ticketmd::GOInfoMap InfoMap;
    return std::move(
        ticketmd::calculateInitialDigestAndDependenciesAndContributions(G,
                                                                        InfoMap)
            ->second);
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
TEST_F(SingleModule, NoCalleeSame) {
  const char *ModuleString = "define internal i32 @foo() { ret i32 1 }\n"
                             "define internal i32 @bar() { ret i32 1 }\n";
  M = parseAssembly(ModuleString);
  const auto &Result = ticketmd::generateTicketMDs(*M);
  EXPECT_TRUE(std::get<0>(Result)) << "Expected Module M to be changed";
  EXPECT_EQ(std::get<1>(Result), 0u) << "Expected zero global variables";
  EXPECT_EQ(std::get<2>(Result), 2u) << "Expected two global functions";
  const Function *Foo = M->getFunction("foo");
  const Function *Bar = M->getFunction("bar");
  EXPECT_TRUE(isEqualDigest(Foo, Bar))
      << "Functions of foo and bar should have the same digest";
}

//
//  IR forming a following call graph for M.
//      foo            bar
//   (return 2)    (return 1)
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
  const auto &Result = ticketmd::generateTicketMDs(*M);
  EXPECT_TRUE(std::get<0>(Result)) << "Expected Module M to be changed";
  EXPECT_EQ(std::get<1>(Result), 0u) << "Expected zero global variables";
  EXPECT_EQ(std::get<2>(Result), 3u) << "Expected three global functions";
  const Function *Foo = M->getFunction("foo");
  const Function *Bar = M->getFunction("bar");
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
  const auto &Result = ticketmd::generateTicketMDs(*M);
  EXPECT_TRUE(std::get<0>(Result)) << "Expected Module M to be changed";
  EXPECT_EQ(std::get<1>(Result), 0u) << "Expected zero global variables";
  EXPECT_EQ(std::get<2>(Result), 4u) << "Expected four global functions";
  const Function *G = M->getFunction("g");
  const Function *P = M->getFunction("p");
  EXPECT_TRUE(isEqualDigest(G, P))
      << "Functions of p and q should have the same digest";
  const Function *Foo = M->getFunction("foo");
  const Function *Bar = M->getFunction("bar");
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
  const auto &Result = ticketmd::generateTicketMDs(*M);
  EXPECT_TRUE(std::get<0>(Result)) << "Expected Module M to be changed";
  EXPECT_EQ(std::get<1>(Result), 0u) << "Expected zero global variables";
  EXPECT_EQ(std::get<2>(Result), 2u) << "Expected two global functions";
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

  const Function *Foo = M->getFunction("foo");
  const ticketmd::GOInfo FooInfo = getGOInfo(Foo);
  Function *Bar = M->getFunction("bar");
  const ticketmd::GOInfo BarInfo = getGOInfo(Bar);

  EXPECT_NE(FooInfo.InitialDigest, BarInfo.InitialDigest)
      << "Expected that functions of foo and bar have the different initial "
         "hash value";

  const auto &FooDependencies = FooInfo.Dependencies;
  const auto &BarDependencies = BarInfo.Dependencies;
  EXPECT_THAT(FooDependencies, ::testing::ElementsAre(Bar))
      << "Expected foo's dependent list is {bar}";
  EXPECT_THAT(BarDependencies, ::testing::ElementsAre(Foo))
      << "Expected bar's dependent list is {foo}";

  const auto &Result = ticketmd::generateTicketMDs(*M);
  EXPECT_TRUE(std::get<0>(Result)) << "Expected Module M to be changed";
  EXPECT_EQ(std::get<1>(Result), 0u) << "Expected zero global variables";
  EXPECT_EQ(std::get<2>(Result), 2u) << "Expected two global functions";
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
  ticketmd::generateTicketMDs(*M);
  const Function *Foo = M->getFunction("foo");
  const Function *Bar = M->getFunction("bar");
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

  Function *Foo = M->getFunction("foo");
  const ticketmd::GOInfo FooInfo = getGOInfo(Foo);
  Function *Bar = M->getFunction("bar");
  const ticketmd::GOInfo BarInfo = getGOInfo(Bar);
  Function *P = M->getFunction("p");
  const ticketmd::GOInfo PInfo = getGOInfo(P);
  Function *Q = M->getFunction("q");
  const ticketmd::GOInfo QInfo = getGOInfo(Q);
  Function *Z = M->getFunction("z");
  const ticketmd::GOInfo ZInfo = getGOInfo(Z);

  EXPECT_EQ(ZInfo.InitialDigest, QInfo.InitialDigest)
      << "Expected that functions of z and q have the same initial "
         "hash value";
  EXPECT_EQ(FooInfo.InitialDigest, BarInfo.InitialDigest)
      << "Expected that functions of foo and bar have the same initial "
         "hash value";
  const ticketmd::DependenciesType &ZDependencies = ZInfo.Dependencies;
  const ticketmd::DependenciesType &PDependencies = PInfo.Dependencies;
  const ticketmd::DependenciesType &QDependencies = QInfo.Dependencies;
  const ticketmd::DependenciesType &FooDependencies = FooInfo.Dependencies;
  const ticketmd::DependenciesType &BarDependencies = BarInfo.Dependencies;

  EXPECT_EQ(ZDependencies.size(), std::size_t{0})
      << "Expected that the size of z's dependent list is zero";
  EXPECT_EQ(QDependencies.size(), std::size_t{0})
      << "Expected that the size of q's dependent list is zero";
  EXPECT_THAT(PDependencies, ::testing::ElementsAre(Z));
  EXPECT_THAT(FooDependencies, ::testing::UnorderedElementsAre(P, Q))
      << "Expected foo's dependent list is {P, Q}";
  EXPECT_THAT(BarDependencies, ::testing::UnorderedElementsAre(P, Q))
      << "Expected bar's dependent list is {P, Q}";

  const auto &Result = ticketmd::generateTicketMDs(*M);
  EXPECT_TRUE(std::get<0>(Result)) << "Expected Module M to be changed";
  EXPECT_EQ(std::get<1>(Result), 0u) << "Expected zero global variables";
  EXPECT_EQ(std::get<2>(Result), 5u) << "Expected five global functions";
  EXPECT_TRUE(isEqualDigest(Foo, Bar)) << "Expected that functions of foo and "
                                          "bar have the same final hash value";
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
// In Module M1, the function 'g' hash is generated by the frontend and 'g' hash
// is generated by the backend. The 'foo' and 'bar' have the same hash value.
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
