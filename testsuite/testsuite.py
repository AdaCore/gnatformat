#! /usr/bin/env python

#
# Copyright (C) 2024, AdaCore
# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
#

import difflib
import os
import shutil
import sys
from os import sep
from pathlib import Path
from typing import override

import e3.env
from e3.testsuite import Testsuite
from e3.testsuite.driver.classic import (
    ProcessResult,
    TestAbortWithError,
    TestAbortWithFailure,
)
from e3.testsuite.driver.diff import (
    DiffTestDriver,
    PatternSubstitute,
    RefiningChain,
    ReplacePath,
)
from e3.testsuite.result import FailureReason, Log, binary_repr, truncated
from gnatcov import GNATcov


class ReplaceBuildVersionAndDate(RefiningChain[str]):
    """
    Return an output refiner to replace the pattern
    'GNATformat <version> (<build-date>)' by 'GNATformat test (test)'
    """

    def __init__(self) -> None:
        super().__init__(
            [PatternSubstitute(r"GNATformat .* \(.*\)", "GNATformat test (test)")]
        )


def valgrind_wrap(env: e3.env.Env, argv: list[str]) -> list[str]:
    """
    If the "--valgrind" option is enabled in the testsuite run, wrap the given
    command line in Valgrind.
    """
    if env.options.valgrind:
        supp = os.path.join(os.path.dirname(__file__), "valgrind.supp")
        argv = [
            "valgrind",
            "-q",
            "--leak-check=full",
            f"--suppressions={supp}",
        ] + argv
    return argv


class GNATformatDriver(DiffTestDriver):
    """
    Driver to run gnatformat with --pipe.

    Usage Instructions:

    1. Place a "test.yaml" file in the test directory with the following keys:
       - driver: "gnatformat"
       - description: A description of the test's purpose
       - args: An array with the arguments to be passed to gnatformat
       - program: optional key to change the program to run (default: "gnatformat");
         used e.g. to exercise the "git-gnatformat" subcommand wrapper
       - status_code: optional key to change the expected status code (default: 0)

    2. Include a "test.out" text file in the test directory with the expected
       results. If a "test.out" file is missing, it will be treated as empty.

    This driver executes the gnatformat binary with the arguments defined in
    test.yaml (defaulting to --pipe) and subsequently verifies its output
    against the expected output in the "test.out" file.
    """

    def run(self):
        # Run the "gnatformat" program (or another, e.g. the "git-gnatformat"
        # subcommand wrapper, when the test overrides "program")...
        program = self.test_env.get("program", "gnatformat")
        argv = [program] + self.test_env.get("args", ["--pipe"])

        # ... on the input Ada source code file
        self.validate_status_code(
            self.shell(valgrind_wrap(self.env, argv), catch_error=False)
        )

    def shell(self, *args, **kwargs) -> ProcessResult:
        if self.env.gnatcov:
            return self.env.gnatcov.decorate_run(super().shell, self, *args, **kwargs)
        else:
            return super().shell(*args, **kwargs)

    def validate_status_code(self, result):
        expected_status_code = self.test_env.get("status_code", 0)
        if expected_status_code != result.status:
            raise TestAbortWithFailure(
                "unexpected exit code {}, expected {}".format(
                    result.status, expected_status_code
                )
            )

    @property
    def output_refiners(self):
        return super().output_refiners + [
            ReplacePath(self.working_dir() + sep),
            ReplaceBuildVersionAndDate(),
        ]

    @property
    @override
    def baseline_file(self) -> tuple[str, bool]:
        """Return the test output baseline file.

        :return: The name of the text file (relative to test directories) that
            contains the expected test output and whether the baseline is a
            regexp.
        """

        default_filename = self.test_env.get("baseline_file", "test.out")

        # On Windows, mostly due to the directory separator, it can be useful
        # to use a different baseline.
        filename = (
            self.test_env.get("windows_baseline_file", default_filename)
            if sys.platform == "win32"
            else default_filename
        )

        is_regexp = self.test_env.get("baseline_regexp", False)

        return (filename, is_regexp)


class GNATformatOnDiskDriver(GNATformatDriver):
    """
    Driver to run gnatformat on sources formatted in place, comparing the
    resulting files byte for byte with baselines.

    The "gnatformat" driver goes through --pipe, which is unsuitable for tests
    about line endings. On Windows, standard output turns every LF into CRLF, so a
    CRLF formatted source comes out as CR CR LF.

    Usage Instructions:

    1. Place a "test.yaml" file in the test directory with the following keys:
       - driver: "gnatformat_on_disk"
       - description: A description of the test's purpose
       - args: An array with the arguments to be passed to gnatformat; they
         must not include --pipe
       - program, status_code: same as for the "gnatformat" driver
       - baselines_dir: optional name of the baselines directory (default:
         "expected")

    2. Include an "expected" directory mirroring the layout of the test
       directory: once gnatformat has run, the formatted "<path>" must be byte
       for byte identical to "expected/<path>". Files without a counterpart
       under "expected" are not checked. Running the testsuite with --rewrite
       overwrites these baselines with the formatted sources (create an empty
       file to bootstrap a new baseline).

    3. Optionally include a "test.out" file with the expected process output
       (e.g. warnings); when it is missing, no output is expected.

    The baselines directory is removed from the working directory before
    gnatformat runs, so that it can never be picked up as a source directory.
    """

    @property
    def baselines_dir(self) -> str:
        return self.test_env.get("baselines_dir", "expected")

    @override
    def run(self):
        program = self.test_env.get("program", "gnatformat")
        args = self.test_env.get("args", [])
        if "--pipe" in args or "-p" in args:
            raise TestAbortWithError(
                "the gnatformat_on_disk driver formats sources in place:"
                " remove --pipe from args"
            )

        # The baselines were copied to the working directory along with the
        # rest of the test directory: remove them so that gnatformat cannot
        # pick them up as sources.
        shutil.rmtree(self.working_dir(self.baselines_dir), ignore_errors=True)

        self.validate_status_code(
            self.shell(valgrind_wrap(self.env, [program] + args), catch_error=False)
        )

    @property
    @override
    def baseline(self) -> tuple[str | None, str | bytes, bool]:
        filename, is_regexp = self.baseline_file
        if not os.path.isfile(self.test_dir(filename)):
            empty: str | bytes = b"" if self.default_encoding == "binary" else ""
            return (self.test_dir(filename), empty, is_regexp)
        return super().baseline

    def baseline_files(self) -> list[Path]:
        """Return the baselines, as paths relative to the baselines directory."""

        root = Path(self.test_dir(self.baselines_dir))
        return sorted(
            path.relative_to(root) for path in root.rglob("*") if path.is_file()
        )

    @override
    def compute_failures(self) -> list[str]:
        # Check the process output against "test.out" (or nothing)

        failures = super().compute_failures()

        # Check each formatted file against its baseline, byte for byte

        baselines = self.baseline_files()
        if not baselines:
            raise TestAbortWithError(
                f"no baseline found in the {self.baselines_dir!r} directory"
            )

        for relative_path in baselines:
            failures.extend(self.compute_bytes_diff(relative_path))

        return failures

    def compute_bytes_diff(self, relative_path: Path) -> list[str]:
        """
        Compare the formatted file at ``relative_path`` (relative to the working
        directory) with its baseline, byte for byte.

        Return the list of failure messages (empty if the file matches),
        logging the diff and rewriting the baseline if requested.
        """

        display_name = relative_path.as_posix()
        baseline_path = Path(self.test_dir(self.baselines_dir)) / relative_path
        actual_path = Path(self.working_dir()) / relative_path

        expected = baseline_path.read_bytes()

        if not actual_path.is_file():
            message = f"missing formatted file: {display_name}"
            self.result.log += f"\n{message}\n"
            self.result.failure_reasons.add(FailureReason.DIFF)
            return [message]

        actual = actual_path.read_bytes()
        if actual == expected:
            return []

        message = f"unexpected content for {display_name}"
        if self.rewrite_baseline:
            baseline_path.write_bytes(actual)
            message += " (baseline updated)"

        # Show the differences with CR and other non-printable bytes escaped,
        # so that line ending differences stand out in the diff.

        def colorize(line: str) -> str:
            if line.startswith("-"):
                color = self.Fore.RED
            elif line.startswith("+"):
                color = self.Fore.GREEN
            elif line.startswith("@"):
                color = self.Fore.CYAN
            else:
                color = ""
            return color + line + self.Style.RESET_ALL

        diff_lines = difflib.unified_diff(
            binary_repr(expected).split("\n"),
            binary_repr(actual).split("\n"),
            fromfile=f"{self.baselines_dir}/{display_name}",
            tofile=display_name,
            n=self.diff_context_size,
            lineterm="",
        )
        diff_log = (
            self.Style.RESET_ALL
            + self.Style.BRIGHT
            + f"Diff failure: {message}\n"
            + "\n".join(colorize(line) for line in diff_lines)
            + "\n"
        )
        self.result.log += "\n" + truncated(
            diff_log, self.testsuite_options.truncate_logs
        )

        # Same bookkeeping as DiffTestDriver.compute_diff: the "expected/out"
        # logs support a single diff, so drop them past the first failure.

        self.failing_diff_count += 1
        if self.failing_diff_count == 1:
            self.result.expected = Log(binary_repr(expected))
            self.result.out = Log(binary_repr(actual))
            self.result.diff = Log(diff_log)
        else:
            self.result.expected = None
            self.result.out = None
            assert isinstance(self.result.diff, Log)
            self.result.diff += "\n" + diff_log

        self.result.failure_reasons.add(FailureReason.DIFF)
        return [message]


class GNATformatTestsuite(Testsuite):
    tests_subdir = "tests"
    test_driver_map = {
        "gnatformat": GNATformatDriver,
        "gnatformat_on_disk": GNATformatOnDiskDriver,
    }

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
        parser.add_argument(
            "--gnatcov",
            nargs="+",
            help="If provided, compute the source code coverage of testcases"
            " on GNATformat. This requires GNATcoverage working with"
            " instrumentation. The argument passed must be a list of"
            " directories that contains all SID files.",
        )
        parser.add_argument(
            "--gnatcov-source-root",
            help="If provided, this will be used as the --source-root gnatcov"
            " CLI argument for producing a Cobertura report with"
            " relative source paths.",
        )
        parser.add_argument(
            "--gnatcov-source-search",
            help="If provided, gnatcov will look for sources in this"
            " directory (recursively) when annotating the coverage"
            " reports. This is needed when the sources are not available"
            " at the location recorded in the SID files, e.g. when"
            " GNATformat was instrumented on another machine.",
        )

    def set_up(self):
        args = self.main.args
        self.env.rewrite_baselines = args.rewrite
        self.env.gnatcov = GNATcov(self) if self.env.options.gnatcov else None

    def tear_down(self) -> None:
        if self.env.gnatcov:
            self.env.gnatcov.report()

        return super().tear_down()


if __name__ == "__main__":
    sys.exit(GNATformatTestsuite().testsuite_main())
