// This test is Linux-only.
// REQUIRES: system-linux

// RUN: rm -rf %t.db
// RUN: env REPOFILE=%t.db clang -c -DFUNC_NAME=func1 -O3 -target x86_64-pc-linux-gnu-repo %S/Inputs/repo_rename_GOs.c -o %t.o
// RUN: env REPOFILE=%t.db repo2obj %t.o --repo %t.db -o %t.elf.o
// RUN: env REPOFILE=%t.db clang -c -DFUNC_NAME=func2 -DDEFINE_OTHER_FUNC -o -O3 -target x86_64-pc-linux-gnu-repo %S/Inputs/repo_rename_GOs.c -o %t1.o
// RUN: env REPOFILE=%t.db repo2obj %t1.o --repo %t.db -o %t1.elf.o
// RUN: env REPOFILE=%t.db clang -c -o -O3 -target x86_64-pc-linux-gnu-repo %s -o %t2.o
// RUN: env REPOFILE=%t.db repo2obj %t2.o --repo %t.db -o %t2.elf.o
// RUN: clang -o %t.out -O3 -target x86_64-pc-linux-gnu-elf %t.elf.o %t1.elf.o %t2.elf.o
// RUN: %t.out | FileCheck %s

extern void func1(void);
extern void other_func(void);
extern void func2(void);

int main () {
    func1 ();
    other_func ();
    func2 ();
}

// CHECK: shared
// CHECK: unique
// CHECK: shared
