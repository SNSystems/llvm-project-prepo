#!/usr/bin/env python3
"""
A little utility for dumping the names and digests from a compilation using its ticket file.

For example:

    $ fragments a.o
    f:9f79e432840075c073e1b70610aabdd0
    str:e88ce1bd9c14b6c379492240050ae7a8
    $

Here `a.o` is a ticket file which references a compilation containing the names 'f' and 'str' with the fragment digests
noted. By default, the tool will dump _all_ of the names from the compilation, but you can request one or more by name:

    $ fragments a.o f
    f:9f79e432840075c073e1b70610aabdd0

The --digest-only (-d) switch suppresses the symbol name, so:

    $ fragments --digest-only a.o f
    9f79e432840075c073e1b70610aabdd0

This can be particularly useful to construct a command-line for the pstore-dump --fragment switch:

    $ pstore-dump --fragment=$(fragments --digest-only a.o f) clang.db

The command-line above will dump the contents of the fragment which corresponds to the name 'f' in 'a.o'.

We can go further and dump multiple fragments by adding the --comma (-c) switch:

    $ pstore-dump --fragment=$(fragments -d -c a.o) clang.db

Note: The utility assumes that both `repo-ticket-dump` and `pstore-dump` are on the PATH.

Dependencies: The PyYAML module must be installed.
"""

import argparse
import yaml
import subprocess


def run(args, verbose=False):
    """
    args: An array of command-line arguments. The first element is the path of the executable to be run.
    verbose: True if verbose output on stdout is desired.

    Returns: The stdout and stderr from the process.
    """

    if verbose:
        print (args)
    output = subprocess.check_output(args, stderr=subprocess.STDOUT, universal_newlines=True)
    if verbose:
        print (output)
    return output


def fragments(ticket, repository, names, verbose=False):
    """
    ticket: path of the ticket file to be dumped.
    repository: Path of the program repository file.
    names: A set of compilation member names to be dumped. If empty, all names are dumped.
    verbose: Produce verbose output?

    Returns: A generator which will produce two-tuples the first member of which is the member
             name and the second its digest.
    """

    digest = run(['repo-ticket-dump', ticket], verbose)
    out = yaml.safe_load(run(['pstore-dump', '--compilation=' + digest.strip(), repository], verbose))

    compilations = out[0]['compilations']
    assert len(compilations) == 1

    return ((member['name'], member['digest'])
            for member in compilations[0]['compilation']['members']
            if len(names) == 0 or member['name'] in names)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Dump a compilation''s fragment digests')
    parser.add_argument('ticket', help='Path of the ticket file')
    parser.add_argument('name', nargs='*', help='Compilation member(s) to be displayed')
    parser.add_argument('-d', '--digest-only', action='store_true', default=False, help='Display the digest only')
    parser.add_argument('-v', '--verbose', action='store_true', default=False, help='Produce verbose output')
    parser.add_argument('-r', '--repository', default='clang.db', help='Path of the program repository')
    parser.add_argument('-c', '--comma', action='store_true', help='Output fields are comma (rather than CR) separated')
    options = parser.parse_args()
    separator = ',' if options.comma else '\n'
    print (separator.join(m[1] if options.digest_only else m[0] + ':' + m[1]
                          for m in fragments(options.ticket,
                                             options.repository,
                                             frozenset(options.name),
                                             options.verbose)))
