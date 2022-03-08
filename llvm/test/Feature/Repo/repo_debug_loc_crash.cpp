// This test checks for a crash.  See issue#175.
// RUN: rm -f %t.db
// RUN: env REPOFILE=%t.db clang -c -g -O2 -target x86_64-pc-linux-musl-repo %s -o %t

struct a {
  virtual void b();
};
class c {
public:
  c(int);
};
int d;
void e(a) { c f(d); }
