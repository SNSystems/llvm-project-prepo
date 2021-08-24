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
// Passing --musl -static
// -----------------------------------------------------------------------------
// RUN: %clang -### -target x86_64-pc-linux-musl-repo -static %s 2>&1 \
// RUN:   | FileCheck -check-prefix=CHECK001 %s
// CHECK001-NOT:  "/usr/local/musl/lib/crti.t.o"
// CHECK001:      "/usr/local/musl/lib/crt1.t.o"
// CHECK001:      "/usr/local/musl/lib/crt1_asm.t.o"
// CHECK001:      "{{.*}}lib{{/|\\\\}}linux{{/|\\\\}}clang_rt.crtbegin-x86_64.o"
// CHECK001:      "{{.*}}lib{{/|\\\\}}linux{{/|\\\\}}clang_rt.crtend-x86_64.o"
// CHECK001:      "-lclang_rt.builtins-x86_64" "-lc"

// -----------------------------------------------------------------------------
// Checking the linked objects and libraries
// Passing --musl -shared
// -----------------------------------------------------------------------------
// RUN: %clang -### -target x86_64-pc-linux-musl-repo -shared %s 2>&1 \
// RUN:   | FileCheck -check-prefix=CHECK002 %s
// CHECK002-NOT:   "/usr/local/musl/lib/crt1.t.o"
// CHECK002-NOT:   "/usr/local/musl/lib/crt1_asm.t.o"
// CHECK002:       "/usr/local/musl/lib/crti.t.o"
// CHECK002:      "{{.*}}lib{{/|\\\\}}linux{{/|\\\\}}clang_rt.crtbegin-x86_64.o"
// CHECK002:      "{{.*}}lib{{/|\\\\}}linux{{/|\\\\}}clang_rt.crtend-x86_64.o"
// CHECK002:       "-lclang_rt.builtins-x86_64" "-lc"

// -----------------------------------------------------------------------------
// Checking the linked objects and libraries
// Passing --musl --sysroot -resource-dir
// -----------------------------------------------------------------------------
// RUN: %clang -### -target x86_64-pc-linux-musl-repo \
// RUN:   -resource-dir=%S/Inputs/resource_dir \
// RUN:   --sysroot=%S/Inputs/basic_linux_libcxx_tree \
// RUN:   %s 2>&1 \
// RUN:   | FileCheck -check-prefix=CHECK003 %s
// CHECK003:      "-resource-dir" "[[RESOURCE_DIR:[^"]+]]"
// CHECK003:      "-isysroot" "[[SYSROOT:[^"]+]]"
// CHECK003-NOT:  "[[SYSROOT]]{{/|\\\\}}lib{{/|\\\\}}crti.t.o"
// CHECK003:      "[[SYSROOT]]{{/|\\\\}}lib{{/|\\\\}}crt1.t.o"
// CHECK003:      "[[SYSROOT]]{{/|\\\\}}lib{{/|\\\\}}crt1_asm.t.o"
// CHECK003:      "[[RESOURCE_DIR]]{{/|\\\\}}lib{{/|\\\\}}linux{{/|\\\\}}clang_rt.crtbegin-x86_64.o"
// CHECK003:      "[[RESOURCE_DIR]]{{/|\\\\}}lib{{/|\\\\}}linux{{/|\\\\}}clang_rt.crtend-x86_64.o"
// CHECK003:      "-lclang_rt.builtins-x86_64" "-lc"

// -----------------------------------------------------------------------------
// Checking the linked objects and libraries
// Passing --musl -static --sysroot
// -----------------------------------------------------------------------------
// RUN: %clang -### -target x86_64-pc-linux-musl-repo -static \
// RUN:   -resource-dir=%S/Inputs/resource_dir \
// RUN:   --sysroot=%S/Inputs/basic_linux_libcxx_tree \
// RUN:   %s 2>&1 \
// RUN:   | FileCheck -check-prefix=CHECK004 %s
// CHECK004:      "-resource-dir" "[[RESOURCE_DIR:[^"]+]]"
// CHECK004:      "-isysroot" "[[SYSROOT:[^"]+]]"
// CHECK004-NOT:  "[[SYSROOT]]{{/|\\\\}}lib{{/|\\\\}}crti.t.o"
// CHECK004:      "[[SYSROOT]]{{/|\\\\}}lib{{/|\\\\}}crt1.t.o"
// CHECK004:      "[[SYSROOT]]{{/|\\\\}}lib{{/|\\\\}}crt1_asm.t.o"
// CHECK004:      "[[RESOURCE_DIR]]{{/|\\\\}}lib{{/|\\\\}}linux{{/|\\\\}}clang_rt.crtbegin-x86_64.o"
// CHECK004:      "[[RESOURCE_DIR]]{{/|\\\\}}lib{{/|\\\\}}linux{{/|\\\\}}clang_rt.crtend-x86_64.o"
// CHECK004:      "-lclang_rt.builtins-x86_64" "-lc"

// -----------------------------------------------------------------------------
// Checking the linked objects and libraries
// Passing --musl -shared --sysroot
// -----------------------------------------------------------------------------
// RUN: %clang -### -target x86_64-pc-linux-musl-repo -shared \
// RUN:   -resource-dir=%S/Inputs/resource_dir \
// RUN:   --sysroot=%S/Inputs/basic_linux_libcxx_tree \
// RUN:   %s 2>&1 \
// RUN:   | FileCheck -check-prefix=CHECK005 %s
// CHECK005:       "-resource-dir" "[[RESOURCE_DIR:[^"]+]]"
// CHECK005:       "-isysroot" "[[SYSROOT:[^"]+]]"
// CHECK005-NOT:   "[[SYSROOT]]{{/|\\\\}}lib{{/|\\\\}}crt1.t.o"
// CHECK005-NOT:   "[[SYSROOT]]{{/|\\\\}}lib{{/|\\\\}}crt1_asm.t.o"
// CHECK005:       "[[SYSROOT]]{{/|\\\\}}lib{{/|\\\\}}crti.t.o"
// CHECK005:       "[[RESOURCE_DIR]]{{/|\\\\}}lib{{/|\\\\}}linux{{/|\\\\}}clang_rt.crtbegin-x86_64.o"
// CHECK005:       "[[RESOURCE_DIR]]{{/|\\\\}}lib{{/|\\\\}}linux{{/|\\\\}}clang_rt.crtend-x86_64.o"
// CHECK005:       "-lclang_rt.builtins-x86_64" "-lc"

// -----------------------------------------------------------------------------
// Checking the linked objects and libraries
// Passing --musl -nostdlib
// -----------------------------------------------------------------------------
// RUN: %clang -### -target x86_64-pc-linux-musl-repo -nostdlib %s 2>&1 \
// RUN:   -resource-dir=%S/Inputs/resource_dir \
// RUN:   | FileCheck -check-prefix=CHECK006 %s
// CHECK006:       "-resource-dir" "[[RESOURCE_DIR:[^"]+]]"
// CHECK006-NOT:   "/usr/local/musl/lib/crti.t.o"
// CHECK006-NOT:   "/usr/local/musl/lib/crt1.t.o"
// CHECK006-NOT:   "/usr/local/musl/lib/crt1_asm.t.o"
// CHECK006-NOT:   "-lclang_rt.builtins-x86_64"
// CHECK006-NOT:   "[[RESOURCE_DIR]]{{/|\\\\}}lib{{/|\\\\}}linux{{/|\\\\}}clang_rt.crtbegin-x86_64.o"
// CHECK006-NOT:   "[[RESOURCE_DIR]]{{/|\\\\}}lib{{/|\\\\}}linux{{/|\\\\}}clang_rt.crtend-x86_64.o"
// CHECK006-NOT:   "-lclang_rt.builtins-x86_64"
// CHECK006-NOT:   "-lc"

// -----------------------------------------------------------------------------
// Checking the linked objects and libraries
// Passing --musl -nostartfiles
// -----------------------------------------------------------------------------
// RUN: %clang -### -target x86_64-pc-linux-musl-repo -nostartfiles \
// RUN:   -resource-dir=%S/Inputs/resource_dir \
// RUN:   --sysroot=%S/Inputs/basic_linux_libcxx_tree \
// RUN:   %s 2>&1 \
// RUN:   | FileCheck -check-prefix=CHECK007 %s
// CHECK007:       "-resource-dir" "[[RESOURCE_DIR:[^"]+]]"
// CHECK007-NOT:   {{.*}}basic_linux_libcxx_tree{{/|\\\\}}lib{{/|\\\\}}crt1_asm.t.o
// CHECK007-NOT:   {{.*}}basic_linux_libcxx_tree{{/|\\\\}}lib{{/|\\\\}}crt1.t.o
// CHECK007-NOT:   {{.*}}basic_linux_libcxx_tree{{/|\\\\}}lib{{/|\\\\}}Scrt1.o
// CHECK007-NOT:   "[[RESOURCE_DIR]]{{/|\\\\}}lib{{/|\\\\}}linux{{/|\\\\}}clang_rt.crtbegin-x86_64.o"
// CHECK007-NOT:   "[[RESOURCE_DIR]]{{/|\\\\}}lib{{/|\\\\}}linux{{/|\\\\}}clang_rt.crtend-x86_64.o"
// CHECK007:       "-lclang_rt.builtins-x86_64" "-lc"

// -----------------------------------------------------------------------------
// Checking the linked objects and libraries
// Passing --musl -nodefaultlibs
// -----------------------------------------------------------------------------
// RUN: %clang -### -target x86_64-pc-linux-musl-repo -nodefaultlibs \
// RUN:   -resource-dir=%S/Inputs/resource_dir \
// RUN:   --sysroot=%S/Inputs/basic_linux_libcxx_tree \
// RUN:   %s 2>&1 \
// RUN:   | FileCheck -check-prefix=CHECK008 %s
// CHECK008:   "-resource-dir" "[[RESOURCE_DIR:[^"]+]]"
// CHECK008:   {{.*}}basic_linux_libcxx_tree{{/|\\\\}}lib{{/|\\\\}}crt1.t.o
// CHECK008:   {{.*}}basic_linux_libcxx_tree{{/|\\\\}}lib{{/|\\\\}}crt1_asm.t.o
// CHECK008:   "[[RESOURCE_DIR]]{{/|\\\\}}lib{{/|\\\\}}linux{{/|\\\\}}clang_rt.crtbegin-x86_64.o"
// CHECK008:   "[[RESOURCE_DIR]]{{/|\\\\}}lib{{/|\\\\}}linux{{/|\\\\}}clang_rt.crtend-x86_64.o"
// CHECK008-NOT:   -lclang_rt.builtins-x86_64
// CHECK008-NOT:   -lc

// -----------------------------------------------------------------------------
// Checking the linked objects and libraries
// Not Passing -fno-use-init-array when musl is selected
// -----------------------------------------------------------------------------
// RUN: %clang -### -target x86_64-pc-linux-musl-repo %s 2>&1 \
// RUN:   | FileCheck -check-prefix=CHECK009 %s
// CHECK009-NOT:          -fno-use-init-array

// -----------------------------------------------------------------------------
// Checking the linked objects and libraries
// c++ when musl is selected
// -----------------------------------------------------------------------------
// RUN: %clangxx -### -target x86_64-pc-linux-musl-repo \
// RUN:   -resource-dir=%S/Inputs/resource_dir \
// RUN:   -ccc-install-dir %S/Inputs/basic_linux_libcxx_tree/bin \
// RUN:   %s 2>&1 \
// RUN:   | FileCheck -check-prefix=CHECK010 %s
// CHECK010:   "-resource-dir" "[[RESOURCE_DIR:[^"]+]]"
// CHECK010:  "-L{{/|\\\\}}usr{{/|\\\\}}local{{/|\\\\}}musl{{/|\\\\}}lib"
// CHECK010:  "-L[[RESOURCE_DIR]]{{/|\\\\}}lib{{/|\\\\}}linux"
// CHECK010:  "-L{{.*}}basic_linux_libcxx_tree{{/|\\\\}}bin{{/|\\\\}}..{{/|\\\\}}lib"
// CHECK010:  "-lc++" "-lc++abi" "-lunwind"