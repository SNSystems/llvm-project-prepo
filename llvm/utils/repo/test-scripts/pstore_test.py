#!/usr/bin/env python
#
# ===- pstore_test.py - --------------------------------------*- python -*--===#
#
# Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
# See https://llvm.org/LICENSE.txt for license information.
# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
#
# ===-----------------------------------------------------------------------===#
''' A utility which automatically tests the built llvm-project-prepo toolchain
    using the pstore unit and system tests. '''
import argparse
import collections
import json
import logging
import os
import shutil
import subprocess
import sys
import pipes
import pytablewriter
import git

from git import Repo

_logger = logging.getLogger(__name__)


class Error(Exception):
    """ Generic 'Error' exception class so that we can raise certain Exceptions
        without also printing out a nasty traceback. """
    pass


def run_cmd(cmd, error_msg, ok_msg, raise_exception=True):
    """ External process runner.

    :param cmd: command that is used to run
    :param error_msg: error message if cmd fails
    :param ok_msg: ok message if cmd successes
    :param raise_exception: a flag to signify whether raise an exception or not

    :returns: a tuple of stdout, stderr and returncode
    """
    try:
        p = subprocess.Popen(
            cmd,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            shell=True,
            universal_newlines=True)
        (stdout, stderr) = p.communicate()
        if p.returncode and raise_exception:
            raise Error('{}"{}" Failed with exit code {}'.format(
                error_msg, stderr, p.returncode))
        elif p.returncode and not raise_exception:
            _logger.info('%s "%s" Failed with exit code %s',
                         error_msg, stderr, p.returncode)
        else:
            _logger.info(ok_msg)
        return (stdout, stderr, p.returncode)
    except subprocess.CalledProcessError as e:
        raise Error('{} Failed with error {}'.format(error_msg, str(e)))


def parse_lit_result(contents, status):
    """
    Parse the output of the lit test runner

    :param contents: the output of the lit test runner
    :status: a dictionary of test status

    :return: the updated dictionary of test status
    """
    for line in contents.splitlines():
        section = line.split()
        if len(section) < 4:
            continue
        if section[0] in ('PASS:', 'XFAIL:'):
            status.setdefault(section[3], []).append('&#10004;')
            status.setdefault(section[3], []).append('&#10004;')
        if section[0] in ('FAIL:', 'XPASS:'):
            status.setdefault(section[3], []).append('&#10060;')
            status.setdefault(section[3], []).append('&#10060;')
        if section[0] == 'UNSUPPORTED:':
            status.setdefault(section[3], []).append('&#10069;')
            status.setdefault(section[3], []).append('&#10069;')
        if section[0] in ('PASS:', 'XFAIL:', 'FAIL:', 'XPASS:', 'UNSUPPORTED:'):
            _logger.info('[SystemTest] %s, %s', section[3], section[0])
    return status


class TestPstore(object):
    """ A class which automatically tests the built llvm-prepo toolchain using
        the pstore unit and system tests. """

    def __init__(self):
        self._options = None
        self._repository = None

    def set_options(self):
        """ Set command line options """
        parser = argparse.ArgumentParser(
            description=
            'Build repo toolchain and run pstore tests targetting repo')
        parser.add_argument(
            '-C',
            '--llvm-configuration',
            default='Debug',
            help='The repo toolchain build configuration')
        parser.add_argument(
            '-P',
            '--pstore-configuration',
            default='Debug',
            help='The pstore project build configuration')
        parser.add_argument(
            '-c',
            '--clone',
            dest='clone',
            action='store_false',
            help=
            'Clone the repo toolchain. If false, pull changes from a remote repository.'
        )
        parser.add_argument(
            '-R',
            '--rebuild-llvm',
            dest='rebuild_llvm',
            action='store_false',
            help=
            'Run the toolchain rebuild step. If false, run the toolchain build step'
        )
        parser.add_argument(
            '-r',
            '--rebuild-pstore',
            dest='rebuild_pstore',
            action='store_false',
            help=
            'Run the pstore rebuild step. If false, run the pstore build step')
        parser.add_argument(
            '-s',
            '--llvm-source-directory',
            default='llvm',
            help='The directory in which the LLVM source code will be cloned.')
        parser.add_argument(
            '-l',
            '--llvm-build-directory',
            default='build_llvm',
            help='The directory in which the llvm build will be created')
        parser.add_argument(
            '-p',
            '--pstore-build-directory',
            default='build_pstore',
            help='The directory in which the pstore build will be created')
        parser.add_argument(
            '-d',
            '--delete-md-file',
            action='store_false',
            help='Delete the existing Markdown file.')
        parser.add_argument(
            '-n',
            '--dry-run',
            action='store_true',
            help='Previews the operation without performing any action.')
        parser.add_argument(
            '-o',
            '--test-result-file',
            default='pstore_test_results.md',
            help=
            'The markdown file in which the pstore test results will be written.'
        )

        self._options = parser.parse_args()

    def set_repository(self):
        """ Set repo toolchain repository dictionary which maps from repository
            name to a repository remote url and a local branch. """
        # Specify the Repository namedtuple.
        repo = collections.namedtuple("Repository",
                                      ["remote_url", "local_branch"])
        repository = {}
        repository['llvm-prepo'] = repo(
            'https://github.com/SNSystems/llvm-prepo.git',
            os.path.abspath(self._options.llvm_source_directory))
        repository['clang-prepo'] = repo(
            'https://github.com/SNSystems/clang-prepo.git',
            os.path.abspath(
                os.path.join(self._options.llvm_source_directory,
                             'tools/clang')))
        repository['pstore'] = repo(
            'https://github.com/SNSystems/pstore.git',
            os.path.abspath(
                os.path.join(self._options.llvm_source_directory,
                             'tools/pstore')))

        _logger.info('[Repository] llvm directory is "%s"',
                     repository['llvm-prepo'].local_branch)
        _logger.info('[Repository] clang directory is "%s"',
                     repository['clang-prepo'].local_branch)
        _logger.info('[Repository] pstore directory is "%s"',
                     repository['pstore'].local_branch)
        self._repository = repository

    def _clone_repo(self, remote_url, path):
        """
        Clone a repo (remote_url) into a given directory (path).

        :param path: a path to the cloned local repository
        :param remote_url: URL of git repo to clone

        :return: no return value
        """
        try:
            _logger.info(
                "[Git] Cloning %s into folder: %s", remote_url, path)
            _logger.info(
                "[Git] Runing: Repo.clone_from(%s, %s)", remote_url, path)
            if not self._options.dry_run:
                Repo.clone_from(remote_url, path)
            _logger.info("[Git] Cloned %s success!", remote_url)
        except git.GitCommandError as e:
            raise Error(
                '[Git] Failed to clone remote_url: {} with error: {} '.format(
                    remote_url, str(e)))

    def _pull_repo(self, path):
        """
        Update the local branch to the remote repository.

        :param path: a path to the local branch
        :return: no return value
        """
        try:
            _logger.info("[Git] Updating %s...", path)
            _logger.info("[Git] Runing: repo = git.Repo(%s)", path)
            _logger.info("[Git] Runing: repo.remotes.origin.pull()")
            if not self._options.dry_run:
                repo = git.Repo(path)
                repo.remotes.origin.pull()
            _logger.info("[Git] Updated %s success!", path)
        except git.GitCommandError as e:
            raise Error('[Git] Failed to update {} with error: {}'.format(
                path, str(e)))

    def git_clone_toolchain(self):
        """ Clone the toolchain repository into a local working directory. """
        try:
            _logger.info("[Git] Cloning...")
            self._clone_repo(self._repository['llvm-prepo'].remote_url,
                             self._repository['llvm-prepo'].local_branch)
            self._clone_repo(self._repository['clang-prepo'].remote_url,
                             self._repository['clang-prepo'].local_branch)
            self._clone_repo(self._repository['pstore'].remote_url,
                             self._repository['pstore'].local_branch)
            _logger.info("[Git] Cloned toolchain success!")
        except Error as e:
            raise e

    def git_update_toolchain(self):
        """ Update the local toolchains branch to the remote repository. """
        try:
            _logger.info("[Git] Updating...")
            self._pull_repo(self._repository['llvm-prepo'].local_branch)
            self._pull_repo(self._repository['clang-prepo'].local_branch)
            self._pull_repo(self._repository['pstore'].local_branch)
            _logger.info("[Git] Updated toolchain success!")
        except Error as e:
            raise e

    def build_toolchain(self):
        """ Build the repo toolchains """
        try:
            # Change the directory to the llvm source directory
            _logger.info('[CMake] Change directory to "%s"',
                         self._repository['llvm-prepo'].local_branch)
            if not self._options.dry_run:
                os.chdir(self._repository['llvm-prepo'].local_branch)

            # Run make_build.py to generate a build.
            make_build_file = os.path.abspath(
                os.path.join(self._repository['pstore'].local_branch,
                             'utils/make_build.py'))
            if not self._options.dry_run and not os.path.exists(
                    make_build_file):
                raise Error(
                    '[CMake] {} does not exist.'.format(make_build_file))

            if self._options.rebuild_llvm:
                _logger.info("[CMake] Generating a toolchain build...")
                cmd = ' '.join((pipes.quote(x) for x in [
                    make_build_file, '-c', self._options.llvm_configuration,
                    '-D', 'LLVM_PARALLEL_LINK_JOBS=1', '-G', 'Ninja', '-o',
                    self._options.llvm_build_directory
                ]))
                _logger.info('[CMake] Running: %s', cmd)

                if not self._options.dry_run:
                    run_cmd(cmd,
                            "[CMake] Failed to generate a toolchain build.",
                            "[CMake] Generated the toolchain successfully.")
            else:
                _logger.info("[CMake] Checking the existing build...")
                if not self._options.dry_run and not os.path.exists(
                        self._options.llvm_build_directory):
                    raise Error(
                        '[CMake] llvm build directory {} does not exist.'.
                        format(self._options.llvm_build_directory))

            # Build the repo toolchain.
            cmd = ' '.join(
                (pipes.quote(x)
                 for x in ['ninja', '-C', self._options.llvm_build_directory]))
            _logger.info('[Ninja] Running: %s', cmd)
            if not self._options.dry_run:
                run_cmd(cmd,
                        "[CMake] Failed to build the llvm repo toolchain.",
                        "[CMake] built the llvm repo toolchain successfully.")
        except Error as e:
            raise e

    def write_repo_json_file(self):
        """ Write a local repo.json config file which is used to config the repo toolchain. """
        json_config_file = os.path.abspath(
            os.path.join(self._repository['llvm-prepo'].local_branch,
                         'utils/repo/repo.json'))
        _logger.info(
            "[Json] copy the %s file to your local build directory and update the file.",
            json_config_file)

        if self._options.dry_run:
            return

        try:
            # read the configs
            with open(json_config_file) as infile:
                config = json.load(infile)

            # update the configs
            repo2obj_path = os.path.abspath(
                os.path.join(self._options.llvm_build_directory, 'bin'))
            os.environ['PATH'] += os.pathsep + repo2obj_path
            _logger.info("[Json] add %s to PATH environment variable",
                         repo2obj_path)

            for key, value in config.items():
                new_key = 'clang++' if key == 'link' else key
                new_value = shutil.which(new_key)
                if not new_value:
                    raise Error(
                        '[Json] could not find cmd "{}"'.format(new_key))
                _logger.info("[Json] update the key %s from %s to %s", key,
                             value, new_value)
                config[key] = new_value

            # write the configs
            with open(
                os.path.abspath(
                    os.path.join(self._repository['pstore'].local_branch,
                                 'repo.json')), 'w') as outfile:
                json.dump(config, outfile)
            _logger.info('[Json] created a local repo.json file successfully.')
        except IOError as e:
            raise Error(
                '[Json] failed to create a local repo.json file: {} '.format(
                    str(e)))

    def _generate_pstore_build(self):
        """ Generate a pstore project build. """
        try:
            _logger.info(
                "[CMake] Generating a pstore build targetting repo...")
            # Change the directory to the llvm source directory
            _logger.info('[CMake] Change directory to "%s"',
                         self._repository['pstore'].local_branch)
            if not self._options.dry_run:
                os.chdir(self._repository['pstore'].local_branch)

            # Run make_build.py to generate a build.
            make_build_file = os.path.abspath(
                os.path.join(self._repository['pstore'].local_branch,
                             'utils/make_build.py'))
            if not self._options.dry_run and not os.path.exists(
                    make_build_file):
                raise Error(
                    '[CMake] {} does not exist.'.format(make_build_file))

            repo_cmake_file = os.path.abspath(
                os.path.join(self._repository['llvm-prepo'].local_branch,
                             'utils/repo/repo.cmake'))
            _logger.info('[CMake] toolchain file is "%s".', repo_cmake_file)
            if not self._options.dry_run and not os.path.exists(
                    repo_cmake_file):
                raise Error(
                    '[CMake] {} does not exist.'.format(repo_cmake_file))

            cmd = ' '.join((pipes.quote(x) for x in [
                make_build_file, '-c', self._options.pstore_configuration,
                '-D', 'PSTORE_EXAMPLES=Yes', '-D',
                'CMAKE_TOOLCHAIN_FILE=%s' % repo_cmake_file, '-D',
                'CMAKE_C_COMPILER:FILEPATH=%s' % os.path.abspath(
                    os.path.join(self._options.llvm_build_directory,
                                 'bin/clang')), '-D',
                'CMAKE_CXX_COMPILER:FILEPATH=%s' % os.path.abspath(
                    os.path.join(self._options.llvm_build_directory,
                                 'bin/clang++')), '-D',
                'utils_dir:STRING=%s' % os.path.abspath(
                    os.path.join(self._repository['llvm-prepo'].local_branch,
                                 'utils/repo')), '-G', 'Ninja', '-o', self.
                _options.pstore_build_directory
            ]))
            _logger.info('[CMake] Running: %s', cmd)
            if not self._options.dry_run:
                run_cmd(cmd, "[CMake] Failed to generate a pstore build.",
                        "[CMake] Generated the pstore successfully.")
        except Error as e:
            raise e

    def _get_cmake_target_in_directory(self, path):
        """
        Converts a pstore directory to a cmake target name.

        :param path: the directory to be converted.
        :return: the cmake target name.
        """
        try:
            if not self._options.dry_run and not os.path.exists(path):
                raise Error(
                    '[CMakeTarget] {} path does not exist.'.format(path))

            if self._options.dry_run and not os.path.exists(path):
                return []

            exclude_target = ['broker', 'common']
            # List the directories in a giving directory (path)
            subdir = list(
                filter(
                    lambda s: os.path.isdir(
                        os.path.abspath(
                            os.path.join(
                                path,
                                s))),
                    os.listdir(path)))
            # List the directories are in the subdir but the name doesn't contain
            # any strings in the exclude_target list.
            target_folder = list(
                filter(lambda s: not any(x in s for x in exclude_target),
                       subdir))
            # List the target relative path to the pstore source directory.
            target_relpath = [
                (lambda t: os.path.relpath(
                    os.path.abspath(
                        os.path.join(
                            path,
                            t)),
                    self._repository['pstore'].local_branch))(t) for t in target_folder]
            # List the cmake target names (without the 'pstore-' prefix).
            from layering import cmake_target_from_path
            target = [(lambda t: cmake_target_from_path(t))(t)
                      for t in target_relpath]
            # List the cmake target names.
            cmake_target = ['pstore-' + x for x in target]
            return cmake_target
        except IOError as e:
            raise Error(
                '[CMakeTarget] failed to get cmake target in directory: {} '.
                format(str(e)))

    def _build_pstore_unit_tests(self, pstore_unit_tests,
                                 pstore_unit_tests_status, clean,
                                 remove_database):
        """
        Build all available pstore unit tests.

        :param pstore_unit_tests: a list of available pstore unit tests.
        :param pstore_unit_tests_status: a dictionary of the pstore unit tests status
        :param clean: a flag to signify whether clean the existing build or not
        :param remove_database: a flag to signify whether delete the database or not

        :return: a dictionary of pstore unit test status
        """
        try:
            if clean:
                # clean the build by deleting all ticket files.
                cmd = ' '.join((pipes.quote(x) for x in [
                    'ninja', '-C', self._options.pstore_build_directory,
                    'clean'
                ]))
                _logger.info('[Ninja] Running: %s', cmd)
                if not self._options.dry_run:
                    run_cmd(cmd, "[Ninja] Failed to clean build.",
                            "[CMake] Clean the build successfully.")

            if remove_database:
                cmd = ' '.join((pipes.quote(x) for x in [
                    'rm', '-f',
                    os.path.abspath(
                        os.path.join(self._options.pstore_build_directory,
                                     'clang.db'))
                ]))
                _logger.info('[Ninja] Running: %s', cmd)
                if not self._options.dry_run:
                    run_cmd(cmd, "[Ninja] Failed to remove the database.",
                            "[CMake] Removed the database successfully.")

            # build and run each pstore unit test.
            for unit_test in pstore_unit_tests:
                cmd = ' '.join((pipes.quote(x) for x in [
                    'ninja', '-C', self._options.pstore_build_directory, unit_test
                ]))
                _logger.info('[Ninja] Building a unit test: %s', cmd)
                if not self._options.dry_run:
                    (_, _, status) = run_cmd(
                        cmd,
                        '[Ninja] Failed to build pstore unit test: %s.' % unit_test,
                        '[Ninja] Built %s unit test successfully' % unit_test,
                        False)
                    # update the build status (Pass: '&#10004;' Fail:
                    # '&#10060;')
                    if status == 0:
                        pstore_unit_tests_status.setdefault(
                            unit_test, []).append('&#10004;')
                    else:
                        pstore_unit_tests_status.setdefault(
                            unit_test, []).append('&#10060;')

            return pstore_unit_tests_status
        except KeyError as e:
            raise Error('[Json] Failed to build pstore unnittests: {}'.format(
                str(e)))

    def _run_pstore_unit_tests(self, pstore_unit_tests,
                               pstore_unit_tests_status):
        """
        Run all available pstore unit tests.

        :param pstore_unit_tests: a list of available pstore unit tests.
        :param pstore_unit_tests_status: a dictionary of the pstore unit tests status

        :return: a dictionary of pstore unit test status
        """
        try:
            # build and run each pstore unit test.
            for unit_test in pstore_unit_tests:
                cmd = os.path.abspath(
                    os.path.join(self._options.pstore_build_directory, 'bin',
                                 unit_test))
                _logger.info('[UnitTest] Running a unit test: %s', cmd)
                if not self._options.dry_run:
                    (_, _, status) = run_cmd(
                        cmd, '[UnitTest] Failed to run a pstore unit test: %s'
                        % unit_test,
                        '[UnitTest] Ran a pstore unit test: %s successfully.' %
                        unit_test, False)
                    # update the run status (Pass: '&#10004;' Fail: '&#10060;')
                    if status == 0:
                        pstore_unit_tests_status.setdefault(
                            unit_test, []).append('&#10004;')
                    else:
                        pstore_unit_tests_status.setdefault(
                            unit_test, []).append('&#10060;')

            return pstore_unit_tests_status
        except KeyError as e:
            raise Error(
                '[UnitTest] Failed to run pstore unnittests: {}'.format(
                    str(e)))

    def _pstore_unit_tests(self, pstore_unit_tests, pstore_unit_tests_status,
                           clean, remove):
        """
        Build and run all available pstore unit tests.

        :param pstore_unit_tests: a list of available pstore unit tests.
        :param pstore_unit_tests_status: a dictionary of the pstore unit tests status
        :param clean: a flag to signify whether clean the existing build or not
        :param remove_database: a flag to signify whether delete the database or not

        :return: a dictionary of pstore unit test status
        """
        try:
            pstore_unit_tests_status = self._build_pstore_unit_tests(
                pstore_unit_tests, pstore_unit_tests_status, clean, remove)
            pstore_unit_tests_status = self._run_pstore_unit_tests(
                pstore_unit_tests, pstore_unit_tests_status)
            return pstore_unit_tests_status
        except Error as e:
            raise e

    def _build_pstore_dependencies(self, pstore_system_test_dependencies):
        """
        Build pstore system test dependencies.

        :param pstore_system_test_dependencies: a list of pstore cmake targets
        which are the dependencies of system test
        :return: no return value
        """
        try:
            # build pstore tools
            for target in pstore_system_test_dependencies:
                cmd = ' '.join((pipes.quote(x) for x in [
                    'ninja', '-C', self._options.pstore_build_directory, target
                ]))
                _logger.info('[Ninja] Build a dependence: %s', cmd)
                if not self._options.dry_run:
                    run_cmd(
                        cmd,
                        "[Ninja] Failed to build a dependence: %s." % target,
                        "[Ninja] Built a dependence: %s successfully." %
                        target)
        except Error as e:
            raise e

    def _pstore_system_tests(self, pstore_system_tests_status):
        """
        Build and run the pstore system tests.

        :param pstore_system_tests_status: a dictionary of pstore system test status
        :return: a dictionary of pstore system test status
        """
        try:
            cmd = ' '.join((pipes.quote(x) for x in [
                'python',
                os.path.abspath(
                    os.path.join(self._repository['llvm-prepo'].local_branch,
                                 'utils/lit/lit.py')), '-v',
                '--no-progress-bar', '--param', 'examples',
                os.path.abspath(
                    os.path.join(self._options.pstore_build_directory, 'bin'))
            ]))
            _logger.info('[SystemTest] Run pstore system tests: %s', cmd)
            if not self._options.dry_run:
                (stdout, _, _) = run_cmd(
                    cmd, "[SystemTest] pstore-system-tests failed.",
                    "[SystemTest] Passed the pstore-system-tests.", False)
                pstore_system_tests_status = parse_lit_result(
                    stdout, pstore_system_tests_status)
            return pstore_system_tests_status
        except Error as e:
            raise e

    def _pstore_tests_summary(self, test_name, test_status, repeat_num):
        """
        Write the test results to a MarkDown file.

        :param test_name: the test name
        :param test_status: a dictionary of the test status
        :param repeat_num: the number of times to build and run the test

        :return: no return value
        """
        try:
            writer = pytablewriter.MarkdownTableWriter()
            writer.table_name = "pstore %s summary "\
            "(pstore with %s configuration) "\
            "(repo toolchain with %s configuration)" % (
                test_name, self._options.pstore_configuration,
                self._options.llvm_configuration)
            _logger.info('[TestReport] Add a table name: %s',
                         writer.table_name)
            writer.header_list = ["Test Name"]
            for time in range(1, repeat_num + 1):
                writer.header_list.append('prepo %s build' % time)
                writer.header_list.append('prepo %s run' % time)
            _logger.info('[TestReport] Add a table header list: %s',
                         writer.header_list)

            if not self._options.dry_run:
                for test_id in test_status:
                    if len(test_status[test_id]) != 2 * repeat_num:
                        raise Error(
                            '[TestReport] {} has incorrect status size.'.
                            format(test_name))

            table = []
            for key, value in test_status.items():
                row = [key]
                row.extend(value)
                table.append(row)
            writer.value_matrix = table
            _logger.info('[TestReport] Add a table: %s', writer.value_matrix)

            # write the output to a file
            _logger.info('[TestReport] Write the table to the %s.',
                         self._options.test_result_file)
            if not self._options.dry_run:
                with open(self._options.test_result_file, "a+") as outfile:
                    writer.stream = outfile
                    writer.write_table()
            _logger.info('[TestReport] Writed the test results successfully!')
        except IOError as e:
            raise Error('[TestReport] Failed to write the test summary: {}'.
                        format(str(e)))

    def _build_run_pstore_tests(self, pstore_unit_tests,
                                pstore_system_test_dependencies, repeat_num):
        """
        Build and run pstore unit and system tests for n times

        :param pstore_unit_tests: a list of available pstore unit tests
        :param pstore_system_test_dependencies: a list of pstore cmake targets
        which are the dependencies of system test
        :param repeat_num: the number of times to build and run pstore tests

        :return: a pair of pstore unit and system test status
        """
        # Dictionary: key: test_name,
        #             values: [1_build_status, 1_run_status, ...
        #                      n_build_status, n_run_status]
        # A list is used to represent the values of the given key.
        # For running multiple times tests, build_status and run_status are used as
        # seperate values instead of a pair of {build_status, run_status}. This is
        # because the list structure is easy to generate the test result table.
        pstore_unit_tests_status = {}
        pstore_system_tests_status = {}

        clean_build = True if self._options.rebuild_pstore else False
        remove_database = True if self._options.rebuild_pstore else False
        for time in range(1, repeat_num + 1):
            _logger.info(
                '[PstoreTest] Pstore tests: time: %s, clean: %s, remove_database: %s',
                time, clean_build, remove_database)
            # Build and run the pstore unit tests.
            _logger.info(
                '[UnitTest] Starting to build and run pstore unit tests...')
            pstore_unit_tests_status = self._pstore_unit_tests(
                pstore_unit_tests, pstore_unit_tests_status, clean_build,
                remove_database)
            _logger.info(
                '[UnitTest] Built and ran pstore unit tests successfully')

            # Build the pstore dependencies.
            _logger.info(
                '[SystemTest] Starting to build the pstore dependencies...')
            self._build_pstore_dependencies(pstore_system_test_dependencies)
            _logger.info(
                '[SystemTest] Built system test dependencies successfully')
            # Run the system tests.
            _logger.info('[SystemTest] Starting to run the system test...')
            pstore_system_tests_status = self._pstore_system_tests(
                pstore_system_tests_status)
            clean_build = True
            remove_database = False
        return (pstore_unit_tests_status, pstore_system_tests_status)

    def test_main(self):
        """ test main entry """
        try:
            exit_code = 0
            build_run_times = 2  # times of building and running pstore tests
            logging.basicConfig(level=logging.DEBUG)
            self.set_options()
            self.set_repository()

            if os.path.exists(self._repository['llvm-prepo'].
                              local_branch) and self._options.clone:
                _logger.info('rmtree "%s"',
                             self._repository['llvm-prepo'].local_branch)
                if not self._options.dry_run:
                    shutil.rmtree(self._repository['llvm-prepo'].local_branch)

            # Clone the llvm, clang and pstore projects
            if self._options.clone:
                self.git_clone_toolchain()
            else:
                self.git_update_toolchain()

            # Prepare to import pstore layering module.
            module_path = os.path.abspath(
                os.path.join(self._repository['pstore'].local_branch,
                             'utils/layering'))
            sys.path.insert(0, module_path)

            if not os.path.isabs(self._options.llvm_build_directory):
                llvm_source_parent_directory = os.path.abspath(
                    os.path.join(self._repository['llvm-prepo'].local_branch,
                                 os.pardir))
                self._options.llvm_build_directory = os.path.abspath(
                    os.path.join(llvm_source_parent_directory,
                                 self._options.llvm_build_directory))
            _logger.info('llvm build directory is "%s"',
                         self._options.llvm_build_directory)

            if not os.path.isabs(self._options.pstore_build_directory):
                self._options.pstore_build_directory = os.path.abspath(
                    os.path.join(self._repository['pstore'].local_branch,
                                 self._options.pstore_build_directory))
            _logger.info('pstore build directory is "%s"',
                         self._options.pstore_build_directory)

            # Remove the existing test report file.
            if not os.path.isabs(self._options.test_result_file):
                self._options.test_result_file = os.path.abspath(
                    self._options.test_result_file)
            _logger.info('pstore test result file is "%s"',
                         self._options.test_result_file)

            if os.path.exists(self._options.test_result_file
                             ) and self._options.delete_md_file:
                _logger.info('rm "%s"', self._options.test_result_file)
                if not self._options.dry_run:
                    os.remove(self._options.test_result_file)

            # Build the repo toolchain.
            self.build_toolchain()

            pstore_unit_tests = self._get_cmake_target_in_directory(
                os.path.abspath(
                    os.path.join(self._repository['pstore'].local_branch,
                                 'unittests')))
            _logger.info('[UnitTest] pstore unit tests: %s', pstore_unit_tests)

            pstore_system_test_dependencies = self._get_cmake_target_in_directory(
                os.path.join(self._repository['pstore'].local_branch, 'tools'))
            pstore_system_test_dependencies.append('pstore-examples')
            _logger.info(
                '[SystemTestDependencies] pstore system tests dependencies: %s',
                pstore_system_test_dependencies)

            # write the local repo.json configure file to the pstore source
            # directory
            self.write_repo_json_file()

            if self._options.rebuild_pstore:
                self._generate_pstore_build()
            else:
                _logger.info(
                    "[PstoreTest] Checking the existing pstore build...")
                if not self._options.dry_run and not os.path.exists(
                        self._options.pstore_build_directory):
                    raise Error(
                        '[PstoreTest] pstore build directory {} does not exist.'
                        .format(self._options.pstore_build_directory))

            # Buld and run the repo unit and system tests.
            (pstore_unit_tests_status,
             pstore_system_tests_status) = self._build_run_pstore_tests(
                 pstore_unit_tests, pstore_system_test_dependencies,
                 build_run_times)

            # write the test results to a MarkDown file.
            _logger.info("[TestReport] writing the test results to %s",
                         self._options.test_result_file)
            self._pstore_tests_summary('unit tests', pstore_unit_tests_status,
                                       build_run_times)
            self._pstore_tests_summary(
                'system tests', pstore_system_tests_status, build_run_times)

        except Error as e:
            _logger.error(e)
            exit_code = 1

        return exit_code


if __name__ == '__main__':
    TEST = TestPstore()
    sys.exit(TEST.test_main())
