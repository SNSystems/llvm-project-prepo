# RUN: rm -f %t.db
# RUN: python %s > %t.ll
# RUN: env REPOFILE=%t.db llc -filetype=obj %t.ll -o %t.o
# RUN: env REPOFILE=%t.db repo2obj %t.o -o %t.elf
# RUN: llvm-readobj -sections %t.elf | FileCheck %s

# CHECK:Sections [
# CHECK:  Section {
# CHECK:    Index: 0
# CHECK:    Name:  (0)
# CHECK:    Type: SHT_NULL (0x0)
# CHECK:    Flags [ (0x0)
# CHECK:    ]
# CHECK:    Address: 0x0
# CHECK:    Offset: 0x0
# CHECK:    Size: 130569
# CHECK:    Link: 0
# CHECK:    Info: 0
# CHECK:    AddressAlignment: 0
# CHECK:    EntrySize: 0
# CHECK:  }
# CHECK:  Section {
# CHECK:    Index: 1
# CHECK:    Name: .strtab (445870)
# CHECK:    Type: SHT_STRTAB (0x3)
# CHECK:    Flags [ (0x0)
# CHECK:    ]
# CHECK:    Address: 0x0
# CHECK:    Offset:
# CHECK:    Size:
# CHECK:    Link: 0
# CHECK:    Info: 0
# CHECK:    AddressAlignment: 0
# CHECK:    EntrySize: 0
# CHECK:  }

# CHECK:  Section {
# CHECK:    Index: 130566
# CHECK:    Name: .group (
# CHECK:    Type: SHT_GROUP (0x11)
# CHECK:    Flags [ (0x0)
# CHECK:    ]
# CHECK:    Address: 0x0
# CHECK:    Offset: 0x
# CHECK:    Size: 8
# CHECK:    Link: 2
# CHECK:    Info: 65283
# CHECK:    AddressAlignment: 4
# CHECK:    EntrySize: 4
# CHECK:  }
# CHECK:  Section {
# CHECK:    Index: 130567
# CHECK:    Name: .text.f9999 (
# CHECK:    Type: SHT_PROGBITS (0x1)
# CHECK:    Flags [ (0x206)
# CHECK:      SHF_ALLOC (0x2)
# CHECK:      SHF_EXECINSTR (0x4)
# CHECK:      SHF_GROUP (0x200)
# CHECK:    ]
# CHECK:    Address: 0x0
# CHECK:    Offset: 0x
# CHECK:    Size:
# CHECK:    Link: 0
# CHECK:    Info: 0
# CHECK:    AddressAlignment: 16
# CHECK:    EntrySize: 0
# CHECK:  }
# CHECK:  Section {
# CHECK:    Index: 130568
# CHECK:    Name: .symtab_shndx (
# CHECK:    Type: SHT_SYMTAB_SHNDX (0x12)
# CHECK:    Flags [ (0x0)
# CHECK:    ]
# CHECK:    Address: 0x0
# CHECK:    Offset: 0x
# CHECK:    Size:
# CHECK:    Link: 2
# CHECK:    Info: 0
# CHECK:    AddressAlignment: 1
# CHECK:    EntrySize: 4
# CHECK:  }
# CHECK:]


import sys

class Digest:
    """Represents a fragment digest."""

    def __init__ (self, v):
        """Turn the numeric value v into an array of 16 byte values (MSB first)."""

        assert v != 0  # 0 is the digest of the compiler's "dummy" section.
        v2a = lambda x, a: v2a (int(x / 256), [int(x % 256)] + a) if x > 0 else a
        arr = v2a (v, [])
        self.__digest = [0] * (16 - len (arr)) + arr

    def __str__ (self):
        """The digest as a IR array of i8."""

        return '[' + ','.join (( ' i8 ' + str (v) for v in self.__digest )) + ' ]'


def write (out, num_to_write):
    """
    Writes an object file with a number of linkonce functions to ensure that we
    can create an ELF file with a large number of sections.
    """

    out.write ('target triple = "x86_64-pc-linux-gnu-repo"\n\n')

    r = range (num_to_write)

    # Write g() whose purpose is to call the linkonce f() functions so that they are
    # preseved in the final output.
    out.write ('define void @g() !repo_ticket !0 {\nentry:\n')
    for ctr in r:
        out.write ('  %{ctr} = call i32 @f{ctr}()\n'.format (ctr=ctr))
    out.write ('  ret void\n}\n')

    # Define many instances of linkonce f().
    for ctr in r:
        out.write ('$f{ctr} = comdat any\n'.format (ctr=ctr))
        out.write ('define linkonce_odr i32 @f{ctr}() comdat !repo_ticket !{ticketno} {{ '
                 'ret i32 {ctr} }}\n'.format (ctr=ctr, ticketno=ctr + 1))

    # Define the repo.tickets metadata.
    out.write ('\n!repo.tickets = !{')
    sep = ''
    for ctr in range (num_to_write + 1):
        out.write ('{sep}!{ctr}'.format (sep=sep, ctr=ctr))
        sep = ',\n' if ctr % 20 == 0 else ','
    out.write ('}\n')

    # Define the TicketNode metadata for each function.

    tn = '!{ticketno} = !TicketNode(name: "{name}", digest: [16 x i8] {digest}, linkage: {linkage}, ' \
         'visibility: default, pruned: false)\n'
    out.write (tn.format (ticketno=0, name="g", digest=Digest (1), linkage='external'))
    for ctr in r:
        out.write (tn.format (ticketno=ctr + 1, name='f' + str(ctr), digest=Digest (ctr + 1), linkage='linkonce_odr'))

if __name__ == '__main__':
    # 0xFF01 is ELF SHN_LORESERVE+1
    write (sys.stdout, 0xFF01)
