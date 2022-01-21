// -----------------------------------------------------------------------------
// Checking the header search
// Passing -musl -stdlib=libc++
// -----------------------------------------------------------------------------
// RUN: %clangxx -### -no-canonical-prefixes -target x86_64-pc-linux-musl-repo -stdlib=libc++ \
// RUN:   -ccc-install-dir %S/Inputs/basic_linux_libcxx_tree/bin \
// RUN:   %s 2>&1 \
// RUN:   | FileCheck --check-prefix=CHECK-X86-64-LIBCXX-MUSL %s

// CHECK-X86-64-LIBCXX-MUSL: "-internal-isystem" "{{.*}}basic_linux_libcxx_tree{{/|\\\\}}bin{{/|\\\\}}..{{/|\\\\}}include{{/|\\\\}}c++{{/|\\\\}}v1"
// CHECK-X86-64-LIBCXX-MUSL: "-internal-isystem" "{{/|\\\\}}usr{{/|\\\\}}local{{/|\\\\}}musl{{/|\\\\}}include"

// -----------------------------------------------------------------------------
// Checking the header search
// Passing -musl -stdlib=libc++ --sysroot
// -----------------------------------------------------------------------------
// RUN: %clangxx -### -target x86_64-pc-linux-musl-repo -stdlib=libc++ \
// RUN:     -ccc-install-dir %S/Inputs/basic_linux_libcxx_tree/bin \
// RUN:     --sysroot=%S/Inputs/basic_linux_libcxx_tree \
// RUN:     %s 2>&1 \
// RUN:   | FileCheck --check-prefix=CHECK-X86-64-LIBCXX-SYSROOT-MUSL %s

// CHECK-X86-64-LIBCXX-SYSROOT-MUSL: "-isysroot" "[[SYSROOT:[^"]+]]"
// CHECK-X86-64-LIBCXX-SYSROOT-MUSL: "-internal-isystem" "{{.*}}basic_linux_libcxx_tree{{/|\\\\}}bin{{/|\\\\}}..{{/|\\\\}}include{{/|\\\\}}c++{{/|\\\\}}v1"
// CHECK-X86-64-LIBCXX-SYSROOT-MUSL: "-internal-isystem" "[[SYSROOT]]{{/|\\\\}}include"

// -----------------------------------------------------------------------------
// Checking the header search
// Passing --musl -nostdinc --sysroot
// -----------------------------------------------------------------------------
// RUN: %clangxx -### -target x86_64-pc-linux-musl-repo -nostdinc \
// RUN:     -ccc-install-dir %S/Inputs/basic_linux_libcxx_tree/bin \
// RUN:     --sysroot=%S/Inputs/basic_linux_libcxx_tree \
// RUN:     %s 2>&1 \
// RUN:   | FileCheck --check-prefix=CHECK-NOSTDINC-SYSROOT %s

// CHECK-NOSTDINC-SYSROOT: "-isysroot" "[[SYSROOT:[^"]+]]"
// CHECK-NOSTDINC-SYSROOT-NOT: "-internal-isystem" "{{.*}}basic_linux_libcxx_tree{{/|\\\\}}bin{{/|\\\\}}..{{/|\\\\}}include{{/|\\\\}}c++{{/|\\\\}}v1"
// CHECK-NOSTDINC-SYSROOT-NOT: "-internal-isystem" "[[SYSROOT]]{{/|\\\\}}include"

// -----------------------------------------------------------------------------
// Checking the header search
// Passing --musl -nobuiltininc --sysroot
// -----------------------------------------------------------------------------
// RUN: %clangxx -### -target x86_64-pc-linux-musl-repo -nobuiltininc \
// RUN:     -ccc-install-dir %S/Inputs/basic_linux_libcxx_tree/bin \
// RUN:     --sysroot=%S/Inputs/basic_linux_libcxx_tree \
// RUN:     %s 2>&1 \
// RUN:   | FileCheck --check-prefix=CHECK-NOBUILTININC-SYSROOT %s

// CHECK-NOBUILTININC: "-isysroot" "[[SYSROOT:[^"]+]]"
// CHECK-NOBUILTININC-SYSROOT-NOT: "-internal-isystem" "{{.*}}basic_linux_libcxx_tree{{/|\\\\}}bin{{/|\\\\}}..{{/|\\\\}}include{{/|\\\\}}c++{{/|\\\\}}v1"
// CHECK-NOBUILTININC-SYSROOT-NOT: "-internal-isystem" "-internal-isystem" "[[SYSROOT]]{{/|\\\\}}include"

// -----------------------------------------------------------------------------
// Checking the header search
// Passing --musl -nostdlibinc --sysroot
// -----------------------------------------------------------------------------
// RUN: %clangxx -### -target x86_64-pc-linux-musl-repo -nostdlibinc \
// RUN:   -ccc-install-dir %S/Inputs/basic_linux_libcxx_tree/bin \
// RUN:   --sysroot=%S/Inputs/basic_linux_libcxx_tree \
// RUN:   %s 2>&1 \
// RUN:   | FileCheck --check-prefix=CHECK-NOSTDLIBINC-SYSROOT %s

// CHECK-NOSTDLIBINC-SYSROOT: "-isysroot" "[[SYSROOT:[^"]+]]"
// CHECK-NOSTDLIBINC-SYSROOT-NOT: "-internal-isystem" "{{.*}}basic_linux_libcxx_tree{{/|\\\\}}bin{{/|\\\\}}..{{/|\\\\}}include{{/|\\\\}}c++{{/|\\\\}}v1"
// CHECK-NOSTDLIBINC-SYSROOT-NOT: "-internal-isystem" "[[SYSROOT]]{{/|\\\\}}include"

// -----------------------------------------------------------------------------
// Checking the linked objects and libraries
// Passing --musl
// -----------------------------------------------------------------------------
// RUN: %clang -### -target x86_64-pc-linux-musl-repo %s 2>&1 \
// RUN:   | FileCheck -check-prefix=CHECK000 %s

// CHECK000:      "{{.*}}rld"
// CHECK000-NOT:  "/usr/local/musl/lib/crti.t"
// CHECK000:      "/usr/local/musl/lib/crt1.t"
// CHECK000:      "/usr/local/musl/lib/crt1_asm.t"
// CHECK000:      "/usr/local/musl/lib/libc_repo.a"
// CHECK000:      "{{.*}}lib{{/|\\\\}}linux{{/|\\\\}}clang_rt.crtbegin-x86_64.o"
// CHECK000:      "{{.*}}lib{{/|\\\\}}linux{{/|\\\\}}clang_rt.crtend-x86_64.o"
// CHECK000:      "-lclang_rt.builtins-x86_64"

// -----------------------------------------------------------------------------
// Checking the linked objects and libraries
// Passing --musl --sysroot
// -----------------------------------------------------------------------------
// RUN: %clang -### -target x86_64-pc-linux-musl-repo \
// RUN:   -resource-dir=%S/Inputs/resource_dir \
// RUN:   --sysroot=%S/Inputs/basic_linux_libcxx_tree \
// RUN:   %s 2>&1 \
// RUN:   | FileCheck -check-prefix=CHECK001 %s

// CHECK001:      "-resource-dir" "[[RESOURCE_DIR:[^"]+]]"
// CHECK001:      "-isysroot" "[[SYSROOT:[^"]+]]"
// CHECK001-NOT:  "[[SYSROOT]]{{/|\\\\}}lib{{/|\\\\}}crti.t"
// CHECK001:      "[[SYSROOT]]{{/|\\\\}}lib{{/|\\\\}}crt1.t"
// CHECK001:      "[[SYSROOT]]{{/|\\\\}}lib{{/|\\\\}}crt1_asm.t"
// CHECK001:      "[[SYSROOT]]{{/|\\\\}}lib{{/|\\\\}}libc_repo.a"
// CHECK001:      "[[RESOURCE_DIR]]{{/|\\\\}}lib{{/|\\\\}}linux{{/|\\\\}}clang_rt.crtbegin-x86_64.o"
// CHECK001:      "[[RESOURCE_DIR]]{{/|\\\\}}lib{{/|\\\\}}linux{{/|\\\\}}clang_rt.crtend-x86_64.o"
// CHECK001:      "-lclang_rt.builtins-x86_64"

// -----------------------------------------------------------------------------
// Checking the linked objects and libraries
// Passing --musl -nostdlib
// -----------------------------------------------------------------------------
// RUN: %clang -### -target x86_64-pc-linux-musl-repo -nostdlib %s 2>&1 \
// RUN:   -resource-dir=%S/Inputs/resource_dir \
// RUN:   | FileCheck -check-prefix=CHECK002 %s

// CHECK002:       "-resource-dir" "[[RESOURCE_DIR:[^"]+]]"
// CHECK002-NOT:   "/usr/local/musl/lib/crt1.t"
// CHECK002-NOT:   "/usr/local/musl/lib/crt1_asm.t"
// CHECK002-NOT:   "/usr/local/musl/lib/libc_repo.a"
// CHECK002-NOT:   "[[RESOURCE_DIR]]{{/|\\\\}}lib{{/|\\\\}}linux{{/|\\\\}}clang_rt.crtbegin-x86_64.o"
// CHECK002-NOT:   "[[RESOURCE_DIR]]{{/|\\\\}}lib{{/|\\\\}}linux{{/|\\\\}}clang_rt.crtend-x86_64.o"
// CHECK002-NOT:   "-lclang_rt.builtins-x86_64"

// -----------------------------------------------------------------------------
// Checking the linked objects and libraries
// Passing --musl -nostartfiles
// -----------------------------------------------------------------------------
// RUN: %clang -### -target x86_64-pc-linux-musl-repo -nostartfiles \
// RUN:   -resource-dir=%S/Inputs/resource_dir \
// RUN:   --sysroot=%S/Inputs/basic_linux_libcxx_tree \
// RUN:   %s 2>&1 \
// RUN:   | FileCheck -check-prefix=CHECK003 %s

// CHECK003:       "-resource-dir" "[[RESOURCE_DIR:[^"]+]]"
// CHECK003-NOT:   {{.*}}basic_linux_libcxx_tree{{/|\\\\}}lib{{/|\\\\}}crt1.t
// CHECK003-NOT:   {{.*}}basic_linux_libcxx_tree{{/|\\\\}}lib{{/|\\\\}}crt1_asm.t
// CHECK003-NOT:   {{.*}}basic_linux_libcxx_tree{{/|\\\\}}lib{{/|\\\\}}libc_repo.a"
// CHECK003-NOT:   "[[RESOURCE_DIR]]{{/|\\\\}}lib{{/|\\\\}}linux{{/|\\\\}}clang_rt.crtbegin-x86_64.o"
// CHECK003-NOT:   "[[RESOURCE_DIR]]{{/|\\\\}}lib{{/|\\\\}}linux{{/|\\\\}}clang_rt.crtend-x86_64.o"
// CHECK003:   "-lclang_rt.builtins-x86_64"

// -----------------------------------------------------------------------------
// Checking the linked objects and libraries
// Passing --musl -nodefaultlibs
// -----------------------------------------------------------------------------
// RUN: %clang -### -target x86_64-pc-linux-musl-repo -nodefaultlibs \
// RUN:   -resource-dir=%S/Inputs/resource_dir \
// RUN:   --sysroot=%S/Inputs/basic_linux_libcxx_tree \
// RUN:   %s 2>&1 \
// RUN:   | FileCheck -check-prefix=CHECK004 %s

// CHECK004:   "-resource-dir" "[[RESOURCE_DIR:[^"]+]]"
// CHECK004:   {{.*}}basic_linux_libcxx_tree{{/|\\\\}}lib{{/|\\\\}}crt1.t
// CHECK004:   {{.*}}basic_linux_libcxx_tree{{/|\\\\}}lib{{/|\\\\}}crt1_asm.t
// CHECK004:   "[[RESOURCE_DIR]]{{/|\\\\}}lib{{/|\\\\}}linux{{/|\\\\}}clang_rt.crtbegin-x86_64.o"
// CHECK004:   "[[RESOURCE_DIR]]{{/|\\\\}}lib{{/|\\\\}}linux{{/|\\\\}}clang_rt.crtend-x86_64.o"
// CHECK004-NOT:   -lclang_rt.builtins-x86_64

// -----------------------------------------------------------------------------
// Checking the linked objects and libraries
// Not Passing -fno-use-init-array when musl is selected
// -----------------------------------------------------------------------------
// RUN: %clang -### -target x86_64-pc-linux-musl-repo %s 2>&1 \
// RUN:   | FileCheck -check-prefix=CHECK005 %s

// RUN: %clang -### -target x86_64-pc-linux-gnu-repo %s 2>&1 \
// RUN:   | FileCheck -check-prefix=CHECK005 %s

// RUN: %clang -### -target x86_64-pc-linux-repo %s 2>&1 \
// RUN:   | FileCheck -check-prefix=CHECK005 %s

// CHECK005-NOT:          -fno-use-init-array

// -----------------------------------------------------------------------------
// Checking the linked objects and libraries
// c++ when musl is selected
// -----------------------------------------------------------------------------
// RUN: %clangxx -### -target x86_64-pc-linux-musl-repo \
// RUN:   -resource-dir=%S/Inputs/resource_dir \
// RUN:   -ccc-install-dir %S/Inputs/basic_linux_libcxx_tree/bin \
// RUN:   %s 2>&1 \
// RUN:   | FileCheck -check-prefix=CHECK006 %s

// CHECK006:   "-resource-dir" "[[RESOURCE_DIR:[^"]+]]"
// CHECK006:  "-L{{/|\\\\}}usr{{/|\\\\}}local{{/|\\\\}}musl{{/|\\\\}}lib"
// CHECK006:  "-L[[RESOURCE_DIR]]{{/|\\\\}}lib{{/|\\\\}}linux"
// CHECK006:  "-L{{.*}}basic_linux_libcxx_tree{{/|\\\\}}bin{{/|\\\\}}..{{/|\\\\}}lib"
// CHECK006:  "-lc++" "-lc++abi"

// -----------------------------------------------------------------------------
// Checking the invoked linker for elf.
// Passing -musl, -gnu and -linux
// -----------------------------------------------------------------------------
// RUN: %clang -### -target x86_64-pc-linux-musl %s 2>&1 \
// RUN:   | FileCheck -check-prefix=CHECK007 %s

// RUN: %clang -### -target x86_64-pc-linux-gnu %s 2>&1 \
// RUN:   | FileCheck -check-prefix=CHECK007 %s

// RUN: %clang -### -target x86_64-pc-linux %s 2>&1 \
// RUN:   | FileCheck -check-prefix=CHECK007 %s

// CHECK007:      "{{.*}}ld"

// -----------------------------------------------------------------------------
// Checking the invoked linker for rld.
// Passing -musl, -gnu and -linux
// -----------------------------------------------------------------------------
// RUN: %clang -### -target x86_64-pc-linux-musl-repo %s 2>&1 \
// RUN:   | FileCheck -check-prefix=CHECK008 %s

// RUN: %clang -### -target x86_64-pc-linux-gnu-repo %s 2>&1 \
// RUN:   | FileCheck -check-prefix=CHECK008 %s

// RUN: %clang -### -target x86_64-pc-linux-repo %s 2>&1 \
// RUN:   | FileCheck -check-prefix=CHECK008 %s

// CHECK008:      "{{.*}}rld"
