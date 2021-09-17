// -----------------------------------------------------------------------------
// Checking the header search
// Passing --musl -stdlib=libc++
// -----------------------------------------------------------------------------
// RUN: %clangxx -### -target x86_64-pc-linux-musl-repo -stdlib=libc++ \
// RUN:   -ccc-install-dir %S/Inputs/basic_linux_libcxx_tree/bin \
// RUN:   -resource-dir=%S/Inputs/resource_dir \
// RUN:   %s 2>&1 \
// RUN:   | FileCheck --check-prefix=CHECK-X86-64-LIBCXX %s
// CHECK-X86-64-LIBCXX: "-resource-dir" "[[RESOURCE_DIR:[^"]+]]"
// CHECK-X86-64-LIBCXX: "-internal-isystem" "{{.*}}basic_linux_libcxx_tree{{/|\\\\}}bin{{/|\\\\}}..{{/|\\\\}}include{{/|\\\\}}c++{{/|\\\\}}v1"
// CHECK-X86-64-LIBCXX: "-internal-isystem" "[[RESOURCE_DIR]]{{(/|\\\\)}}include"
// CHECK-X86-64-LIBCXX: "-internal-isystem" "{{/|\\\\}}usr{{/|\\\\}}local{{/|\\\\}}musl{{/|\\\\}}include"

// -----------------------------------------------------------------------------
// Checking the header search
// Passing --musl -stdlib=libc++ --sysroot
// -----------------------------------------------------------------------------
// RUN: %clangxx -### -target x86_64-pc-linux-musl-repo -stdlib=libc++ \
// RUN:     -ccc-install-dir %S/Inputs/basic_linux_libcxx_tree/bin \
// RUN:     -resource-dir=%S/Inputs/resource_dir \
// RUN:     --sysroot=%S/Inputs/basic_linux_libcxx_tree \
// RUN:     %s 2>&1 \
// RUN:   | FileCheck --check-prefix=CHECK-X86-64-LIBCXX-SYSROOT %s
// CHECK-X86-64-LIBCXX-SYSROOT: "-resource-dir" "[[RESOURCE_DIR:[^"]+]]"
// CHECK-X86-64-LIBCXX-SYSROOT: "-isysroot" "[[SYSROOT:[^"]+]]"
// CHECK-X86-64-LIBCXX-SYSROOT: "-internal-isystem" "{{.*}}basic_linux_libcxx_tree{{/|\\\\}}bin{{/|\\\\}}..{{/|\\\\}}include{{/|\\\\}}c++{{/|\\\\}}v1"
// CHECK-X86-64-LIBCXX-SYSROOT: "-internal-isystem" "[[RESOURCE_DIR]]{{(/|\\\\)}}include"
// CHECK-X86-64-LIBCXX-SYSROOT: "-internal-isystem" "[[SYSROOT]]{{/|\\\\}}include"

// -----------------------------------------------------------------------------
// Checking the header search
// Passing --musl -nostdinc --sysroot
// -----------------------------------------------------------------------------
// RUN: %clangxx -### -target x86_64-pc-linux-musl-repo -nostdinc \
// RUN:     -ccc-install-dir %S/Inputs/basic_linux_libcxx_tree/bin \
// RUN:     -resource-dir=%S/Inputs/resource_dir \
// RUN:     --sysroot=%S/Inputs/basic_linux_libcxx_tree \
// RUN:     %s 2>&1 \
// RUN:   | FileCheck --check-prefix=CHECK-NOSTDINC-SYSROOT %s
// CHECK-NOSTDINC-SYSROOT: "-resource-dir" "[[RESOURCE_DIR:[^"]+]]"
// CHECK-NOSTDINC-SYSROOT: "-isysroot" "[[SYSROOT:[^"]+]]"
// CHECK-NOSTDINC-SYSROOT-NOT: "-internal-isystem" "{{.*}}basic_linux_libcxx_tree{{/|\\\\}}bin{{/|\\\\}}..{{/|\\\\}}include{{/|\\\\}}c++{{/|\\\\}}v1"
// CHECK-NOSTDINC-SYSROOT-NOT: "-internal-isystem" "[[RESOURCE_DIR]]{{/|\\\\}}include"
// CHECK-NOSTDINC-SYSROOT-NOT: "-internal-isystem" "-internal-isystem" "[[SYSROOT]]{{/|\\\\}}include"

// -----------------------------------------------------------------------------
// Checking the header search
// Passing --musl -nobuiltininc --sysroot
// -----------------------------------------------------------------------------
// RUN: %clangxx -### -target x86_64-pc-linux-musl-repo -nobuiltininc \
// RUN:     -ccc-install-dir %S/Inputs/basic_linux_libcxx_tree/bin \
// RUN:     -resource-dir=%S/Inputs/resource_dir \
// RUN:     --sysroot=%S/Inputs/basic_linux_libcxx_tree \
// RUN:     %s 2>&1 \
// RUN:   | FileCheck --check-prefix=CHECK-NOBUILTININC-SYSROOT %s
// CHECK-NOBUILTININC: "-resource-dir" "[[RESOURCE_DIR:[^"]+]]"
// CHECK-NOBUILTININC: "-isysroot" "[[SYSROOT:[^"]+]]"
// CHECK-NOBUILTININC-SYSROOT-NOT: "-internal-isystem" "{{.*}}basic_linux_libcxx_tree{{/|\\\\}}bin{{/|\\\\}}..{{/|\\\\}}include{{/|\\\\}}c++{{/|\\\\}}v1"
// CHECK-NOBUILTININC-SYSROOT-NOT: "-internal-isystem" "[[RESOURCE_DIR]]{{/|\\\\}}include"
// CHECK-NOBUILTININC-SYSROOT-NOT: "-internal-isystem" "-internal-isystem" "[[SYSROOT]]{{/|\\\\}}include"

// -----------------------------------------------------------------------------
// Checking the header search
// Passing --musl -nostdlibinc --sysroot
// -----------------------------------------------------------------------------
// RUN: %clangxx -### -target x86_64-pc-linux-musl-repo -nostdlibinc \
// RUN:   -ccc-install-dir %S/Inputs/basic_linux_libcxx_tree/bin \
// RUN:   -resource-dir=%S/Inputs/resource_dir \
// RUN:   --sysroot=%S/Inputs/basic_linux_libcxx_tree \
// RUN:   %s 2>&1 \
// RUN:   | FileCheck --check-prefix=CHECK-NOSTDLIBINC-SYSROOT %s
// CHECK-NOSTDLIBINC-SYSROOT: "-resource-dir" "[[RESOURCE_DIR:[^"]+]]"
// CHECK-NOSTDLIBINC-SYSROOT: "-isysroot" "[[SYSROOT:[^"]+]]"
// CHECK-NOSTDLIBINC-SYSROOT-NOT: "-internal-isystem" "{{.*}}basic_linux_libcxx_tree{{/|\\\\}}bin{{/|\\\\}}..{{/|\\\\}}include{{/|\\\\}}c++{{/|\\\\}}v1"
// CHECK-NOSTDLIBINC-SYSROOT: "-internal-isystem" "[[RESOURCE_DIR]]{{/|\\\\}}include"
// CHECK-NOSTDLIBINC-SYSROOT-NOT: "-internal-isystem" "[[SYSROOT]]{{/|\\\\}}include"

// -----------------------------------------------------------------------------
// Checking the linked objects and libraries
// Passing --musl
// -----------------------------------------------------------------------------
// RUN: %clang -### -target x86_64-pc-linux-musl-repo %s 2>&1 \
// RUN:   | FileCheck -check-prefix=CHECK000 %s
// CHECK000-NOT:  "/usr/local/musl/lib/crti.t.o"
// CHECK000:      "/usr/local/musl/lib/crt1.t.o"
// CHECK000:      "/usr/local/musl/lib/crt1_asm.t.o"
// CHECK000:      "{{.*}}lib{{/|\\\\}}linux{{/|\\\\}}clang_rt.crtbegin-x86_64.o"
// CHECK000:      "{{.*}}lib{{/|\\\\}}linux{{/|\\\\}}clang_rt.crtend-x86_64.o"
// CHECK000:      "-lclang_rt.builtins-x86_64" "-lc"

// -----------------------------------------------------------------------------
// Checking the linked objects and libraries
// Passing --musl --sysroot -resource-dir
// -----------------------------------------------------------------------------
// RUN: %clang -### -target x86_64-pc-linux-musl-repo \
// RUN:   -resource-dir=%S/Inputs/resource_dir \
// RUN:   --sysroot=%S/Inputs/basic_linux_libcxx_tree \
// RUN:   %s 2>&1 \
// RUN:   | FileCheck -check-prefix=CHECK001 %s
// CHECK001:      "-resource-dir" "[[RESOURCE_DIR:[^"]+]]"
// CHECK001:      "-isysroot" "[[SYSROOT:[^"]+]]"
// CHECK001-NOT:  "[[SYSROOT]]{{/|\\\\}}lib{{/|\\\\}}crti.t.o"
// CHECK001:      "[[SYSROOT]]{{/|\\\\}}lib{{/|\\\\}}crt1.t.o"
// CHECK001:      "[[SYSROOT]]{{/|\\\\}}lib{{/|\\\\}}crt1_asm.t.o"
// CHECK001:      "[[RESOURCE_DIR]]{{/|\\\\}}lib{{/|\\\\}}linux{{/|\\\\}}clang_rt.crtbegin-x86_64.o"
// CHECK001:      "[[RESOURCE_DIR]]{{/|\\\\}}lib{{/|\\\\}}linux{{/|\\\\}}clang_rt.crtend-x86_64.o"
// CHECK001:      "-lclang_rt.builtins-x86_64" "-lc"

// -----------------------------------------------------------------------------
// Checking the linked objects and libraries
// Passing --musl -nostdlib
// -----------------------------------------------------------------------------
// RUN: %clang -### -target x86_64-pc-linux-musl-repo -nostdlib %s 2>&1 \
// RUN:   -resource-dir=%S/Inputs/resource_dir \
// RUN:   | FileCheck -check-prefix=CHECK002 %s
// CHECK002:       "-resource-dir" "[[RESOURCE_DIR:[^"]+]]"
// CHECK002-NOT:   "/usr/local/musl/lib/crti.t.o"
// CHECK002-NOT:   "/usr/local/musl/lib/crt1.t.o"
// CHECK002-NOT:   "/usr/local/musl/lib/crt1_asm.t.o"
// CHECK002-NOT:   "-lclang_rt.builtins-x86_64"
// CHECK002-NOT:   "[[RESOURCE_DIR]]{{/|\\\\}}lib{{/|\\\\}}linux{{/|\\\\}}clang_rt.crtbegin-x86_64.o"
// CHECK002-NOT:   "[[RESOURCE_DIR]]{{/|\\\\}}lib{{/|\\\\}}linux{{/|\\\\}}clang_rt.crtend-x86_64.o"
// CHECK002-NOT:   "-lclang_rt.builtins-x86_64"
// CHECK002-NOT:   "-lc"

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
// CHECK003-NOT:   {{.*}}basic_linux_libcxx_tree{{/|\\\\}}lib{{/|\\\\}}crt1_asm.t.o
// CHECK003-NOT:   {{.*}}basic_linux_libcxx_tree{{/|\\\\}}lib{{/|\\\\}}crt1.t.o
// CHECK003-NOT:   {{.*}}basic_linux_libcxx_tree{{/|\\\\}}lib{{/|\\\\}}Scrt1.o
// CHECK003-NOT:   "[[RESOURCE_DIR]]{{/|\\\\}}lib{{/|\\\\}}linux{{/|\\\\}}clang_rt.crtbegin-x86_64.o"
// CHECK003-NOT:   "[[RESOURCE_DIR]]{{/|\\\\}}lib{{/|\\\\}}linux{{/|\\\\}}clang_rt.crtend-x86_64.o"
// CHECK003:       "-lclang_rt.builtins-x86_64" "-lc"

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
// CHECK004:   {{.*}}basic_linux_libcxx_tree{{/|\\\\}}lib{{/|\\\\}}crt1.t.o
// CHECK004:   {{.*}}basic_linux_libcxx_tree{{/|\\\\}}lib{{/|\\\\}}crt1_asm.t.o
// CHECK004:   "[[RESOURCE_DIR]]{{/|\\\\}}lib{{/|\\\\}}linux{{/|\\\\}}clang_rt.crtbegin-x86_64.o"
// CHECK004:   "[[RESOURCE_DIR]]{{/|\\\\}}lib{{/|\\\\}}linux{{/|\\\\}}clang_rt.crtend-x86_64.o"
// CHECK004-NOT:   -lclang_rt.builtins-x86_64
// CHECK004-NOT:   -lc

// -----------------------------------------------------------------------------
// Checking the linked objects and libraries
// Not Passing -fno-use-init-array when musl is selected
// -----------------------------------------------------------------------------
// RUN: %clang -### -target x86_64-pc-linux-musl-repo %s 2>&1 \
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
// CHECK006:  "-lc++" "-lc++abi" "-lunwind"
