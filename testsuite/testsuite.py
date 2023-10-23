#! /usr/bin/env python

from __future__ import annotations

import sys

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


class GenDocDriver(DiffTestDriver):
    """
    Run the "gendoc" test program to generate both the Prettier Document and
    the reformatted Ada source code for a given input, checking that both match
    baselines.

    Testcases must provide 3 files:

    * ``ada`` to containt the Ada source code to process;
    * ``expected_ir`` to contain the baseline for the expected Document;
    * ``expected_ada`` to contain the baseline for the reformatted Ada source
      code.

    If the Ada source code needs another grammar rule than the default one for
    parsing, it must be set in the ``test.yaml`` file's ``grammar_rule`` key.
    For instance: ``grammar_rule: expr``.
    """

    @property
    def ada_file(self) -> str:
        """
        Return the absolute path to the Ada source code input file.
        """
        return self.test_dir("ada")

    @property
    def expected_ir_file(self) -> str:
        """
        Return the absolute path to the IR (Prettier Document) baseline file.
        """
        return self.test_dir("expected_ir")

    @property
    def expected_ada_file(self) -> str:
        """
        Return the absolute path to the reformatted Ada source code baseline
        file.
        """
        return self.test_dir("expected_ada")

    @property
    def output_file(self):
        """
        Return the absolute path to the file in which to store the reformatted
        Ada source code ("actual").
        """
        return self.working_dir("reformatted_ada")

    def run(self):
        # Run the "gendoc" program...
        argv = ["gendoc"]

        # ... on the input Ada source code file
        argv.append(self.ada_file)

        # ... potentially using a custom grammar rule for parsing
        if "grammar_rule" in self.test_env:
            argv += ["-r", self.test_env["grammar_rule"]]

        # ... writing reformatted Ada source code
        argv += ["-o", self.output_file]

        self.shell(valgrind_wrap(self.env, argv))

    @staticmethod
    def read_file(filename: str) -> str:
        """
        Convenience method to read a text file.
        """
        with open(filename) as f:
            return f.read()

    def compute_failures(self) -> list[str]:
        # Check both the Prettier Document against a baseline, but also the
        # reformatted Ada source code against its own baseline.
        return (
            self.compute_diff(
                self.expected_ir_file,
                self.read_file(self.expected_ir_file),
                self.output.log,
                "unexpected Prettier Document",
            ) + self.compute_diff(
                self.expected_ada_file,
                self.read_file(self.expected_ada_file),
                self.read_file(self.output_file),
                "unexpected reformatted code",
            )
        )


class GNATfmtTestsuite(Testsuite):
    tests_subdir = "tests"
    test_driver_map = {"gendoc": GenDocDriver}

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
    sys.exit(GNATfmtTestsuite().testsuite_main())
