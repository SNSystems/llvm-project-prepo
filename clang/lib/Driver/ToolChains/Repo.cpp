//===--- Repo.cpp - Repo ToolChain Implementations --------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "Repo.h"
#include "CommonArgs.h"
#include "clang/Driver/Compilation.h"
#include "clang/Driver/DriverDiagnostic.h"
#include "llvm/Support/VirtualFileSystem.h"

using namespace clang::driver;
using namespace clang::driver::tools;
using namespace clang::driver::toolchains;
using namespace clang;
using namespace llvm::opt;

static void constructRepoMuslLinkArgs(Compilation &C, const JobAction &JA,
                                      const toolchains::RepoMuslToolChain &RTC,
                                      const InputInfo &Output,
                                      const InputInfoList &Inputs,
                                      const ArgList &Args,
                                      ArgStringList &CmdArgs,
                                      const char *LinkingOutput) {

  const Driver &D = RTC.getDriver();

  //----------------------------------------------------------------------------
  //
  //----------------------------------------------------------------------------

  const std::string MuslRoot =
      D.SysRoot.empty() ? "/usr/local/musl" : D.SysRoot;
  if (!Args.hasArg(options::OPT_nostartfiles, options::OPT_nostdlib)) {
    CmdArgs.push_back(Args.MakeArgString(MuslRoot + "/lib/crt1.t"));
    CmdArgs.push_back(Args.MakeArgString(MuslRoot + "/lib/crt1_asm.t"));
    CmdArgs.push_back(Args.MakeArgString(MuslRoot + "/lib/libc_repo.a"));
  }

  // If compiler-rt is built inside of LLVM project, we could find its path
  // using getCompilerRTPath function. If build compiler-rt as stand-alone
  // project and give --sysroot=preference, use sysroot as the libary root.
  SmallString<128> CRTPath(RTC.getCompilerRTPath());
  if (!llvm::sys::fs::exists(Twine(CRTPath))) {
    CRTPath.clear();
    llvm::sys::path::append(CRTPath, D.SysRoot.empty() ? "/usr" : D.SysRoot);
    llvm::sys::path::append(CRTPath, "/lib/linux");
  }
  if (!Args.hasArg(options::OPT_nostartfiles, options::OPT_nostdlib)) {
    auto CRTBegin = CRTPath;
    llvm::sys::path::append(CRTBegin, "/clang_rt.crtbegin-x86_64.o");
    auto CRTEnd = CRTPath;
    llvm::sys::path::append(CRTEnd, "/clang_rt.crtend-x86_64.o");
    CmdArgs.push_back(Args.MakeArgString(RTC.GetFilePath(CRTBegin.c_str())));
    CmdArgs.push_back(Args.MakeArgString(RTC.GetFilePath(CRTEnd.c_str())));
  }

  //----------------------------------------------------------------------------
  // Library Search Paths
  //----------------------------------------------------------------------------
  CmdArgs.push_back(Args.MakeArgString(StringRef("-L") + MuslRoot + "/lib"));

  if (RTC.getVFS().exists(CRTPath))
    CmdArgs.push_back(Args.MakeArgString("-L" + CRTPath));

  for (const auto &LibPath : RTC.getFilePaths())
    CmdArgs.push_back(Args.MakeArgString(StringRef("-L") + LibPath));

  //----------------------------------------------------------------------------
  //
  //----------------------------------------------------------------------------
  Args.AddAllArgs(CmdArgs,
                  {options::OPT_T_Group, options::OPT_e, options::OPT_s,
                   options::OPT_t, options::OPT_u_Group});

  //----------------------------------------------------------------------------
  // Libraries
  //----------------------------------------------------------------------------
  if (!Args.hasArg(options::OPT_nostdlib, options::OPT_nodefaultlibs)) {
    CmdArgs.push_back("-lclang_rt.builtins-x86_64");
  }

  if (D.CCCIsCXX() && RTC.ShouldLinkCXXStdlib(Args)) {
    RTC.AddCXXStdlibLibArgs(Args, CmdArgs);
  }
}

void repo::Linker::ConstructJob(Compilation &C, const JobAction &JA,
                                const InputInfo &Output,
                                const InputInfoList &Inputs,
                                const ArgList &Args,
                                const char *LinkingOutput) const {

  const ToolChain &TC = getToolChain();
  ArgStringList CmdArgs;

  AddLinkerInputs(TC, Inputs, Args, CmdArgs, JA);
  CmdArgs.push_back("-o");
  CmdArgs.push_back(Output.getFilename());

  if (getToolChain().getTriple().isMusl()) {
    constructRepoMuslLinkArgs(
        C, JA,
        static_cast<const toolchains::RepoMuslToolChain &>(getToolChain()),
        Output, Inputs, Args, CmdArgs, LinkingOutput);
  }
  C.addCommand(std::make_unique<Command>(
      JA, *this, ResponseFileSupport::AtFileCurCP(),
      Args.MakeArgString(TC.GetLinkerPath()), CmdArgs, Inputs));
}

RepoMuslToolChain::RepoMuslToolChain(const Driver &D,
                                     const llvm::Triple &Triple,
                                     const llvm::opt::ArgList &Args)
    : ToolChain(D, Triple, Args) {
  getProgramPaths().push_back(D.getInstalledDir());
  if (D.getInstalledDir() != D.Dir)
    getProgramPaths().push_back(D.Dir);

  if (!D.SysRoot.empty()) {
    SmallString<128> P(D.SysRoot);
    llvm::sys::path::append(P, "lib");
    getFilePaths().push_back(std::string(P.str()));
  }

  getFilePaths().push_back(D.Dir + "/../lib");
}

RepoMuslToolChain::~RepoMuslToolChain() = default;

void RepoMuslToolChain::AddCXXStdlibLibArgs(const ArgList &Args,
                                            ArgStringList &CmdArgs) const {
  CmdArgs.push_back("-lc++");
  CmdArgs.push_back("-lc++abi");
}

Tool *RepoMuslToolChain::buildLinker() const {
  return new tools::repo::Linker(*this);
}

bool RepoMuslToolChain::isPICDefault() const { return false; }

bool RepoMuslToolChain::isPIEDefault() const { return false; }

bool RepoMuslToolChain::isPICDefaultForced() const { return false; }

void RepoMuslToolChain::addClangTargetOptions(const ArgList &DriverArgs,
                                              ArgStringList &CC1Args,
                                              Action::OffloadKind) const {
  if (!DriverArgs.hasFlag(options::OPT_fuse_init_array,
                          options::OPT_fno_use_init_array, true))
    CC1Args.push_back("-fno-use-init-array");
}

void RepoMuslToolChain::AddClangSystemIncludeArgs(
    const ArgList &DriverArgs, ArgStringList &CC1Args) const {
  if (DriverArgs.hasArg(options::OPT_nostdinc))
    return;

  const Driver &D = getDriver();

  // Add the Clang builtin headers (<resource>/include).
  if (!DriverArgs.hasArg(options::OPT_nobuiltininc)) {
    llvm::SmallString<128> P = llvm::StringRef(D.ResourceDir);
    llvm::sys::path::append(P, "/include");
    addSystemInclude(DriverArgs, CC1Args, P);
  }

  if (DriverArgs.hasArg(options::OPT_nostdlibinc))
    return;

  if (!DriverArgs.hasArg(options::OPT_nostdinc) &&
      !DriverArgs.hasArg(options::OPT_nostdlibinc) &&
      !DriverArgs.hasArg(options::OPT_nobuiltininc) &&
      !DriverArgs.hasArg(options::OPT_nostdincxx)) {
    // On repo, libc++ is installed alongside the compiler in include/c++/v1, so
    // get from '<install>/bin' to '<install>/include/c++/v1'.
    llvm::SmallString<128> P = llvm::StringRef(D.getInstalledDir());
    llvm::sys::path::append(P, "..", "include", "c++", "v1");
    addSystemInclude(DriverArgs, CC1Args, P);
  }

  if (!DriverArgs.hasArg(options::OPT_nobuiltininc)) {
    if (!D.SysRoot.empty()) {
      SmallString<128> P(D.SysRoot);
      llvm::sys::path::append(P, "include");
      addSystemInclude(DriverArgs, CC1Args, P.str());
    } else {
      addSystemInclude(DriverArgs, CC1Args, "/usr/local/musl/include");
    }
  }
}

ToolChain::CXXStdlibType
RepoMuslToolChain::GetCXXStdlibType(const ArgList &Args) const {
  if (Arg *const A = Args.getLastArg(options::OPT_stdlib_EQ)) {
    StringRef Value = A->getValue();
    if (Value == "libstdc++")
      return ToolChain::CST_Libstdcxx;
    if (Value == "libc++")
      return ToolChain::CST_Libcxx;
    getDriver().Diag(diag::err_drv_invalid_stdlib_name) << A->getAsString(Args);
  }
  return ToolChain::CST_Libcxx;
}
