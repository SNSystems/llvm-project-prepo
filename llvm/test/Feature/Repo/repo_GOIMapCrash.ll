; Test for a bug in  'RepoMetadataGenerationPass' which caused a crash.
; RUN: rm -f %t.db
; RUN: env REPOFILE=%t.db opt -debug-only ir %s -S -o %t 2>&1 | FileCheck %s

; REQUIRES: asserts

target triple = "x86_64-pc-linux-gnu-repo"

@Test = global [47 x i8*] [i8* @A.1, i8* @A.2, i8* @A.3, i8* @A.4, i8* @A.5, i8* @A.6, i8* @A.7, i8* @A.8, i8* @A.9, i8* @A.10, i8* @A.11, i8* @A.12, i8* @A.13, i8* @A.14, i8* @A.15, i8* @A.16, i8* @A.17, i8* @A.18, i8* @A.19, i8* @A.20, i8* @A.21, i8* @A.22, i8* @A.23, i8* @A.24, i8* @A.25, i8* @A.26, i8* @A.27, i8* @A.28, i8* @A.29, i8* @A.30, i8* @A.31, i8* @A.32, i8* @A.33, i8* @A.34, i8* @A.35, i8* @A.36, i8* @A.37, i8* @A.38, i8* @A.39, i8* @A.40, i8* @A.41, i8* @A.42, i8* @A.43, i8* @A.44, i8* @A.45, i8* @A.46, i8* @A.47]

@A.1 = internal constant i8 1
@A.2 = internal constant i8 2
@A.3 = internal constant i8 3
@A.4 = internal constant i8 4
@A.5 = internal constant i8 5
@A.6 = internal constant i8 6
@A.7 = internal constant i8 7
@A.8 = internal constant i8 8
@A.9 = internal constant i8 9
@A.10 = internal constant i8 10
@A.11 = internal constant i8 11
@A.12 = internal constant i8 12
@A.13 = internal constant i8 13
@A.14 = internal constant i8 14
@A.15 = internal constant i8 15
@A.16 = internal constant i8 16
@A.17 = internal constant i8 17
@A.18 = internal constant i8 18
@A.19 = internal constant i8 19
@A.20 = internal constant i8 20
@A.21 = internal constant i8 21
@A.22 = internal constant i8 22
@A.23 = internal constant i8 23
@A.24 = internal constant i8 24
@A.25 = internal constant i8 25
@A.26 = internal constant i8 26
@A.27 = internal constant i8 27
@A.28 = internal constant i8 28
@A.29 = internal constant i8 29
@A.30 = internal constant i8 30
@A.31 = internal constant i8 31
@A.32 = internal constant i8 32
@A.33 = internal constant i8 33
@A.34 = internal constant i8 34
@A.35 = internal constant i8 35
@A.36 = internal constant i8 36
@A.37 = internal constant i8 37
@A.38 = internal constant i8 38
@A.39 = internal constant i8 39
@A.40 = internal constant i8 40
@A.41 = internal constant i8 41
@A.42 = internal constant i8 42
@A.43 = internal constant i8 43
@A.44 = internal constant i8 44
@A.45 = internal constant i8 45
@A.46 = internal constant i8 46
@A.47 = internal constant i8 47

; CHECK: GO Name:Test
; CHECK: Initial Dependencies: [ ]
; CHECK: Contributions: [ A.1,A.2,A.3,A.4,A.5,A.6,A.7,A.8,A.9,A.10,A.11,A.12,A.13,A.14,A.15,A.16,A.17,A.18,A.19,A.20,A.21,A.22,A.23,A.24,A.25,A.26,A.27,A.28,A.29,A.30,A.31,A.32,A.33,A.34,A.35,A.36,A.37,A.38,A.39,A.40,A.41,A.42,A.43,A.44,A.45,A.46,A.47]
; CHECK: GO Name:A.1
; CHECK: Initial Dependencies: [ ]
; CHECK: GO Name:A.2
; CHECK: Initial Dependencies: [ ]
; CHECK: GO Name:A.3
; CHECK: Initial Dependencies: [ ]
; CHECK: GO Name:A.4
; CHECK: Initial Dependencies: [ ]
; CHECK: GO Name:A.5
; CHECK: Initial Dependencies: [ ]
; CHECK: GO Name:A.6
; CHECK: Initial Dependencies: [ ]
; CHECK: GO Name:A.7
; CHECK: Initial Dependencies: [ ]
; CHECK: GO Name:A.8
; CHECK: Initial Dependencies: [ ]
; CHECK: GO Name:A.9
; CHECK: Initial Dependencies: [ ]
; CHECK: GO Name:A.10
; CHECK: Initial Dependencies: [ ]
; CHECK: GO Name:A.11
; CHECK: Initial Dependencies: [ ]
; CHECK: GO Name:A.12
; CHECK: Initial Dependencies: [ ]
; CHECK: GO Name:A.13
; CHECK: Initial Dependencies: [ ]
; CHECK: GO Name:A.14
; CHECK: Initial Dependencies: [ ]
; CHECK: GO Name:A.15
; CHECK: Initial Dependencies: [ ]
; CHECK: GO Name:A.16
; CHECK: Initial Dependencies: [ ]
; CHECK: GO Name:A.17
; CHECK: Initial Dependencies: [ ]
; CHECK: GO Name:A.18
; CHECK: Initial Dependencies: [ ]
; CHECK: GO Name:A.19
; CHECK: Initial Dependencies: [ ]
; CHECK: GO Name:A.20
; CHECK: Initial Dependencies: [ ]
; CHECK: GO Name:A.21
; CHECK: Initial Dependencies: [ ]
; CHECK: GO Name:A.22
; CHECK: Initial Dependencies: [ ]
; CHECK: GO Name:A.23
; CHECK: Initial Dependencies: [ ]
; CHECK: GO Name:A.24
; CHECK: Initial Dependencies: [ ]
; CHECK: GO Name:A.25
; CHECK: Initial Dependencies: [ ]
; CHECK: GO Name:A.26
; CHECK: Initial Dependencies: [ ]
; CHECK: GO Name:A.27
; CHECK: Initial Dependencies: [ ]
; CHECK: GO Name:A.28
; CHECK: Initial Dependencies: [ ]
; CHECK: GO Name:A.29
; CHECK: Initial Dependencies: [ ]
; CHECK: GO Name:A.30
; CHECK: Initial Dependencies: [ ]
; CHECK: GO Name:A.31
; CHECK: Initial Dependencies: [ ]
; CHECK: GO Name:A.32
; CHECK: Initial Dependencies: [ ]
; CHECK: GO Name:A.33
; CHECK: Initial Dependencies: [ ]
; CHECK: GO Name:A.34
; CHECK: Initial Dependencies: [ ]
; CHECK: GO Name:A.35
; CHECK: Initial Dependencies: [ ]
; CHECK: GO Name:A.36
; CHECK: Initial Dependencies: [ ]
; CHECK: GO Name:A.37
; CHECK: Initial Dependencies: [ ]
; CHECK: GO Name:A.38
; CHECK: Initial Dependencies: [ ]
; CHECK: GO Name:A.39
; CHECK: Initial Dependencies: [ ]
; CHECK: GO Name:A.40
; CHECK: Initial Dependencies: [ ]
; CHECK: GO Name:A.41
; CHECK: Initial Dependencies: [ ]
; CHECK: GO Name:A.42
; CHECK: Initial Dependencies: [ ]
; CHECK: GO Name:A.43
; CHECK: Initial Dependencies: [ ]
; CHECK: GO Name:A.44
; CHECK: Initial Dependencies: [ ]
; CHECK: GO Name:A.45
; CHECK: Initial Dependencies: [ ]
; CHECK: GO Name:A.46
; CHECK: Initial Dependencies: [ ]
; CHECK: GO Name:A.47
; CHECK: Initial Dependencies: [ ]
; CHECK: GO Name:Test
; CHECK: Final Dependencies: [ A.1,A.2,A.3,A.4,A.5,A.6,A.7,A.8,A.9,A.10,A.11,A.12,A.13,A.14,A.15,A.16,A.17,A.18,A.19,A.20,A.21,A.22,A.23,A.24,A.25,A.26,A.27,A.28,A.29,A.30,A.31,A.32,A.33,A.34,A.35,A.36,A.37,A.38,A.39,A.40,A.41,A.42,A.43,A.44,A.45,A.46,A.47]
; CHECK: GO Name:A.1
; CHECK: Final Dependencies: [ Test]
; CHECK: GO Name:A.2
; CHECK: Final Dependencies: [ Test]
; CHECK: GO Name:A.3
; CHECK: Final Dependencies: [ Test]
; CHECK: GO Name:A.4
; CHECK: Final Dependencies: [ Test]
; CHECK: GO Name:A.5
; CHECK: Final Dependencies: [ Test]
; CHECK: GO Name:A.6
; CHECK: Final Dependencies: [ Test]
; CHECK: GO Name:A.7
; CHECK: Final Dependencies: [ Test]
; CHECK: GO Name:A.8
; CHECK: Final Dependencies: [ Test]
; CHECK: GO Name:A.9
; CHECK: Final Dependencies: [ Test]
; CHECK: GO Name:A.10
; CHECK: Final Dependencies: [ Test]
; CHECK: GO Name:A.11
; CHECK: Final Dependencies: [ Test]
; CHECK: GO Name:A.12
; CHECK: Final Dependencies: [ Test]
; CHECK: GO Name:A.13
; CHECK: Final Dependencies: [ Test]
; CHECK: GO Name:A.14
; CHECK: Final Dependencies: [ Test]
; CHECK: GO Name:A.15
; CHECK: Final Dependencies: [ Test]
; CHECK: GO Name:A.16
; CHECK: Final Dependencies: [ Test]
; CHECK: GO Name:A.17
; CHECK: Final Dependencies: [ Test]
; CHECK: GO Name:A.18
; CHECK: Final Dependencies: [ Test]
; CHECK: GO Name:A.19
; CHECK: Final Dependencies: [ Test]
; CHECK: GO Name:A.20
; CHECK: Final Dependencies: [ Test]
; CHECK: GO Name:A.21
; CHECK: Final Dependencies: [ Test]
; CHECK: GO Name:A.22
; CHECK: Final Dependencies: [ Test]
; CHECK: GO Name:A.23
; CHECK: Final Dependencies: [ Test]
; CHECK: GO Name:A.24
; CHECK: Final Dependencies: [ Test]
; CHECK: GO Name:A.25
; CHECK: Final Dependencies: [ Test]
; CHECK: GO Name:A.26
; CHECK: Final Dependencies: [ Test]
; CHECK: GO Name:A.27
; CHECK: Final Dependencies: [ Test]
; CHECK: GO Name:A.28
; CHECK: Final Dependencies: [ Test]
; CHECK: GO Name:A.29
; CHECK: Final Dependencies: [ Test]
; CHECK: GO Name:A.30
; CHECK: Final Dependencies: [ Test]
; CHECK: GO Name:A.31
; CHECK: Final Dependencies: [ Test]
; CHECK: GO Name:A.32
; CHECK: Final Dependencies: [ Test]
; CHECK: GO Name:A.33
; CHECK: Final Dependencies: [ Test]
; CHECK: GO Name:A.34
; CHECK: Final Dependencies: [ Test]
; CHECK: GO Name:A.35
; CHECK: Final Dependencies: [ Test]
; CHECK: GO Name:A.36
; CHECK: Final Dependencies: [ Test]
; CHECK: GO Name:A.37
; CHECK: Final Dependencies: [ Test]
; CHECK: GO Name:A.38
; CHECK: Final Dependencies: [ Test]
; CHECK: GO Name:A.39
; CHECK: Final Dependencies: [ Test]
; CHECK: GO Name:A.40
; CHECK: Final Dependencies: [ Test]
; CHECK: GO Name:A.41
; CHECK: Final Dependencies: [ Test]
; CHECK: GO Name:A.42
; CHECK: Final Dependencies: [ Test]
; CHECK: GO Name:A.43
; CHECK: Final Dependencies: [ Test]
; CHECK: GO Name:A.44
; CHECK: Final Dependencies: [ Test]
; CHECK: GO Name:A.45
; CHECK: Final Dependencies: [ Test]
; CHECK: GO Name:A.46
; CHECK: Final Dependencies: [ Test]
; CHECK: GO Name:A.47
; CHECK: Final Dependencies: [ Test]
