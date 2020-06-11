#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
Enables the use of tools which don't yet understand the Program Repository with
repository ticket files.

Each ticket file specified on the command line is converted to a traditional
ELF object file (using the repo2obj tool). The resulting ELF file is that passed
to the tool which operates as normal. Any additional command-line arguments are
passed to the tool as-is.
"""

from __future__ import print_function

import argparse
import errno
import json
import logging
import os
import subprocess
import sys

TICKET_FILE_SIZE = 24

_logger = logging.getLogger(__name__)


def _safe_stat(path):
    try:
        return os.stat(path)
    except OSError as ex:
        if ex.errno == errno.ENOENT:
            return None
        raise


def _run(args):
    """

    :param args: An array of arguments for the process (the first element is the executable path)
    :return: Nothing
    """

    _logger.info(args)
    try:
        out = subprocess.check_output(args, stderr=subprocess.STDOUT).decode('utf-8')
    except OSError as ex:
        if ex.errno == errno.ENOENT:
            raise RuntimeError('executable was not found ({0})'.format(args[0]))
        raise
    except subprocess.CalledProcessError as cpe:
        _logger.error(cpe.output)
        raise
    print(out, end='')
    return 0


def _repo2obj(repo2obj_path, src, dest):
    """
    Runs the repo2obj tool -- which converts a program repository ticket file
    to a traditional ELF object file -- reading from the ticket file 'src' and
    writing its output to 'dest'.

    :param repo2obj_path:
    :param src:
    :param dest:
    :return: Nothing
    """

    cmd = [repo2obj_path, src, '-o', dest]
    repofile = os.getenv('REPOFILE')
    if repofile is not None:
        cmd.extend(['--repo', repofile])
    return _run(cmd)


def _run_real_tool(tool_path, args):
    return _run([tool_path] + args)


def wrap_tool(tool, argv):
    """

    :param tool: Name of the tool executable to be run. This path of this executable is located in the configuration file.
    :param argv: The command-line argument array.
    :return: Nothing
    """

    with _open_config_file(name='repo.json') as fp:
        config = json.load (fp)
    repo2obj_path = config ['repo2obj']
    tool_path = config [tool]

    new_args = []
    for arg in argv:
        if arg == '--_verbose':
            logging.getLogger('').setLevel(logging.DEBUG)
            continue

        if len (arg) == 0 or arg[0] != '-':
            # A positional argument
            input_path = arg
            input_stat = _safe_stat(input_path)
            if input_stat is None or input_stat.st_size != TICKET_FILE_SIZE:
                _logger.info('skipping "%s"', input_path)
            else:
                input_dir, input_name = os.path.split(input_path)
                output_path = os.path.join(input_dir, input_name + '.elf')
                output_stat = _safe_stat(output_path)
                if output_stat is None or input_stat.st_mtime > output_stat.st_mtime:
                    _repo2obj(repo2obj_path, input_path, output_path)
                arg = output_path
        new_args.append (arg)

    return _run_real_tool(tool_path, new_args)


def _open_config_file(name='repo.json'):
    """
    Searches up the directory hierarchy for a named file.
    :param name: The name of the file to be found.
    :return: The path of a file with the specified name or None if not found.
    """

    prev_path = None
    path = os.path.abspath(os.curdir)
    while path != prev_path:
        try:
            fp = open(os.path.join (path, name))
            _logger.debug('configuration file "%s" was found at "%s"', name, path)
            return fp
        except:
            pass
        # Navigate one level up the directory hierarchy
        prev_path = path
        path = os.path.abspath(os.path.join(path, os.pardir))

    _logger.warning('configuration file "%s" was not found in any parent directory', name)
    return None

# eof: wrap_tool.py
