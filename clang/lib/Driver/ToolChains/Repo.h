//===--- Repo.h - Repo ToolChain Implementations ----------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_LIB_DRIVER_TOOLCHAINS_REPO_H
#define LLVM_CLANG_LIB_DRIVER_TOOLCHAINS_REPO_H

#include "Linux.h"
#include "clang/Driver/Tool.h"
#include "clang/Driver/ToolChain.h"

namespace clang {
namespace driver {
namespace tools {
namespace repo {

/// A class to directly call repo linker when targeting on *-repo.
class LLVM_LIBRARY_VISIBILITY Linker : public Tool {
public:
  Linker(const ToolChain &TC) : Tool("repo::Linker", "rld", TC) {}

  bool hasIntegratedCPP() const override { return false; }
  bool isLinkJob() const override { return true; }

  void ConstructJob(Compilation &C, const JobAction &JA,
                    const InputInfo &Output, const InputInfoList &Inputs,
                    const llvm::opt::ArgList &TCArgs,
                    const char *LinkingOutput) const override;
};
} // namespace repo
} // namespace tools

namespace toolchains {

/// RepoMuslToolChain - A tool chain using the repo tools and repo-based
/// musl-libc and llvm libc++ libraries to perform compilation and linking
/// commands. Currently does not support repo-based glibc library but
/// compilation is fine.
/// Since the *-musl-* toolchain doesn't support in the upstream LLVM, we need
/// to add this class when targetting on repo and using musl libc linrary. We
/// could add the musl toolchain support (like gnu toolchain).
/// TODO: support musl toolcahin in upstream LLVM.
class LLVM_LIBRARY_VISIBILITY RepoMuslToolChain : public ToolChain {
  friend class Linux;

protected:
  Tool *buildLinker() const override;

public:
  RepoMuslToolChain(const Driver &D, const llvm::Triple &Triple,
                    const llvm::opt::ArgList &Args);
  ~RepoMuslToolChain() override;

  bool isPICDefault() const override;
  bool isPIEDefault() const override;
  bool isPICDefaultForced() const override;

  void
  addClangTargetOptions(const llvm::opt::ArgList &DriverArgs,
                        llvm::opt::ArgStringList &CC1Args,
                        Action::OffloadKind DeviceOffloadKind) const override;
  void
  AddClangSystemIncludeArgs(const llvm::opt::ArgList &DriverArgs,
                            llvm::opt::ArgStringList &CC1Args) const override;

  const char *getDefaultLinker() const override { return "rld"; }

  CXXStdlibType GetCXXStdlibType(const llvm::opt::ArgList &Args) const override;

  void AddCXXStdlibLibArgs(const llvm::opt::ArgList &Args,
                           llvm::opt::ArgStringList &CmdArgs) const override;

  bool IsIntegratedAssemblerDefault() const override { return true; }
};

} // end namespace toolchains
} // end namespace driver
} // end namespace clang

#endif // LLVM_CLANG_LIB_DRIVER_TOOLCHAINS_REPO_H
