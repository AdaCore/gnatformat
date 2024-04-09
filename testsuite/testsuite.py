#! /usr/bin/env python

from __future__ import annotations

import sys

import e3.env
from e3.testsuite import Testsuite
from e3.testsuite.driver.diff import DiffTestDriver


def valgrind_wrap(env: e3.env.Env, argv: list[str]) -> list[str]:
    """
    If the "--valgrind" option is enabled in the testsuite run, wrap the given
    command line in Valgrind.
    """
    if env.options.valgrind:
        argv = ["valgrind", "-q", "--leak-check=full"] + argv
    return argv


class GNATformatDriver(DiffTestDriver):
    """
    Driver to run gnatformat.

    Usage Instructions:

    1. Place a "test.yaml" file in the test directory with the following keys:
       - description: A description of the test's purpose
       - args: An array with the arguments to be passed to gnatformat

    2. Include a "test.out" text file in the test directory with the expected
       results. If a "test.out" file is missing, it will be treated as empty.

    This driver executes the gnatformat binary with the arguments defined in
    test.yaml (defaulting to --pipe) and subsequently verifies its output
    against the expected output in the "test.out" file.
    """

    def run(self):
        # Run the "gnatformat" program...
        argv = ["gnatformat"] + self.test_env.get("args", ["--pipe"])

        # ... on the input Ada source code file
        self.shell(valgrind_wrap(self.env, argv))


class GNATformatTestsuite(Testsuite):
    tests_subdir = "tests"
    test_driver_map = {"gnatformat": GNATformatDriver}

    def add_options(self, parser):
        parser.add_argument(
            "--valgrind",
            action="store_true",
            help="Run tests with Valgrind to check memory issues.",
        )
        parser.add_argument(
            "--rewrite",
            "-r",
            action="store_true",
            help="Rewrite test baselines according to current output.",
        )

    def set_up(self):
        args = self.main.args
        self.env.rewrite_baselines = args.rewrite


if __name__ == "__main__":
    sys.exit(GNATformatTestsuite().testsuite_main())
