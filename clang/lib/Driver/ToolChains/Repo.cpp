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

static void constructRepoLinkArgs(Compilation &C, const JobAction &JA,
                                  const toolchains::RepoMuslToolChain &RTC,
                                  const InputInfo &Output,
                                  const InputInfoList &Inputs,
                                  const ArgList &Args, ArgStringList &CmdArgs,
                                  const char *LinkingOutput) {

  const Driver &D = RTC.getDriver();

  //----------------------------------------------------------------------------
  //
  //----------------------------------------------------------------------------
  CmdArgs.push_back("-o");
  CmdArgs.push_back(Output.getFilename());

  std::string MuslRoot = D.SysRoot.empty() ? "/usr/local/musl" : D.SysRoot;
  if (!Args.hasArg(options::OPT_nostartfiles, options::OPT_nostdlib)) {
    CmdArgs.push_back(Args.MakeArgString(MuslRoot + "/lib/crt1.t"));
    CmdArgs.push_back(Args.MakeArgString(MuslRoot + "/lib/crt1_asm.t"));
    CmdArgs.push_back(Args.MakeArgString(MuslRoot + "/lib/libc_repo.a"));
  }

  auto CRTPath = RTC.getCompilerRTPath();
  if (!RTC.getVFS().exists(CRTPath)) // Build compiler-rt as stand-alone
    CRTPath = "/usr/lib/linux";
  if (!Args.hasArg(options::OPT_nostartfiles, options::OPT_nostdlib)) {
    CmdArgs.push_back(
        Args.MakeArgString(CRTPath + "/clang_rt.crtbegin-x86_64.o"));
    CmdArgs.push_back(
        Args.MakeArgString(CRTPath + "/clang_rt.crtend-x86_64.o"));
  }

  //----------------------------------------------------------------------------
  // Library Search Paths
  //----------------------------------------------------------------------------
  CmdArgs.push_back(Args.MakeArgString(StringRef("-L") + MuslRoot + "/lib"));

  if (RTC.getVFS().exists(CRTPath))
    CmdArgs.push_back(Args.MakeArgString("-L" + CRTPath));

  const ToolChain::path_list &LibPaths = RTC.getFilePaths();
  for (const auto &LibPath : LibPaths)
    CmdArgs.push_back(Args.MakeArgString(StringRef("-L") + LibPath));

  //----------------------------------------------------------------------------
  //
  //----------------------------------------------------------------------------
  Args.AddAllArgs(CmdArgs,
                  {options::OPT_T_Group, options::OPT_e, options::OPT_s,
                   options::OPT_t, options::OPT_u_Group});
  AddLinkerInputs(RTC, Inputs, Args, CmdArgs, JA);

  //----------------------------------------------------------------------------
  // Libraries
  //----------------------------------------------------------------------------
  if (!Args.hasArg(options::OPT_nostdlib, options::OPT_nodefaultlibs)) {
    CmdArgs.push_back("-lclang_rt.builtins-x86_64");
  }

  if (D.CCCIsCXX()) {
    if (RTC.ShouldLinkCXXStdlib(Args))
      RTC.AddCXXStdlibLibArgs(Args, CmdArgs);
  }

  return;
}

void repo::Linker::ConstructJob(Compilation &C, const JobAction &JA,
                                const InputInfo &Output,
                                const InputInfoList &Inputs,
                                const ArgList &Args,
                                const char *LinkingOutput) const {
  auto &RTC =
      static_cast<const toolchains::RepoMuslToolChain &>(getToolChain());

  ArgStringList CmdArgs;
  constructRepoLinkArgs(C, JA, RTC, Output, Inputs, Args, CmdArgs,
                        LinkingOutput);

  const char *Exec = Args.MakeArgString(RTC.GetLinkerPath());
  C.addCommand(std::make_unique<Command>(
      JA, *this, ResponseFileSupport::AtFileCurCP(), Exec, CmdArgs, Inputs));
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

RepoMuslToolChain::~RepoMuslToolChain() {}

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

  if (DriverArgs.hasArg(options::OPT_nostdlibinc))
    return;

  if (!DriverArgs.hasArg(options::OPT_nostdinc) &&
      !DriverArgs.hasArg(options::OPT_nostdlibinc) &&
      !DriverArgs.hasArg(options::OPT_nobuiltininc) &&
      !DriverArgs.hasArg(options::OPT_nostdincxx)) {
    // On musl-repo, libc++ is installed alongside the compiler in
    // include/c++/v1, so get from '<install>/bin' to
    // '<install>/include/c++/v1'.
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
  Arg *A = Args.getLastArg(options::OPT_stdlib_EQ);
  if (A) {
    StringRef Value = A->getValue();
    if (Value == "libstdc++")
      return ToolChain::CST_Libstdcxx;
    else if (Value == "libc++")
      return ToolChain::CST_Libcxx;
    else
      getDriver().Diag(diag::err_drv_invalid_stdlib_name)
          << A->getAsString(Args);
  }
  return ToolChain::CST_Libcxx;
}
