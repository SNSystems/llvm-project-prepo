#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# ===- archive.py - ------------------------------------------*- python -*--===#
#
# Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
# See https://llvm.org/LICENSE.txt for license information.
# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
#
# ===-----------------------------------------------------------------------===#

from __future__ import print_function

import subprocess
import logging
import sys

import wrap_tool

def main (argv):
    try:
        logging.basicConfig()
        return wrap_tool.wrap_tool(tool='ar', argv=argv[1:])
    except subprocess.CalledProcessError:
        return 1

if __name__ == '__main__':
    sys.exit(main(sys.argv))
# eof: archive.py
