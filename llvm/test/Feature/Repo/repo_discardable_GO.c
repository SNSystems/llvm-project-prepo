// RUN: rm -rf %t.db
// RUN: env REPOFILE=%t.db clang -c -O3 -target x86_64-pc-linux-gnu-repo %s -o %t.o
// RUN: env REPOFILE=%t.db repo-ticket-dump %t.o > %t.log
// RUN: env REPOFILE=%t.db clang -c -O3 -target x86_64-pc-linux-gnu-repo %S/Inputs/repo_external_GO.c -o %t1.o
// RUN: env REPOFILE=%t.db clang -c -O3 -target x86_64-pc-linux-gnu-repo %s -o %t2.o
// RUN: env REPOFILE=%t.db repo-ticket-dump %t2.o > %t2.log
// RUN: diff %t.log %t2.log

static int f0() {
  return 1;
}

int f1() {
  return f0();
}
