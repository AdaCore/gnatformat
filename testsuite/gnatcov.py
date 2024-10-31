import glob
import logging
import os.path
import shutil
from typing import Callable

from e3.os.process import Run, quote_arg
from e3.testsuite.driver import TestDriver
from e3.testsuite.driver.classic import ProcessResult


class GNATcov:
    """
    Helper to compute code coverage with GNATcoverage using the testsuite.

    After initialization, the workflow for this helper is:

    * use the `decorate_run()` method to run a program that contributes to code
      coverage (i.e. that instrumented programs);
    * call the `report()` method to generate coverage reports.
    """

    def __init__(self, testsuite, coverage_level="stmt+decision"):
        """
        :param GNATformatTestsuite testsuite: Testsuite instance to work with.
        :param str covlevel: Coverage level to pass to GNATcoverage.
        """
        self.coverage_level = coverage_level
        self.sid_dirs = testsuite.env.options.gnatcov

        self.temp_dir = os.path.join(testsuite.env.working_dir, "gnatcov")
        self.traces_dir = os.path.join(self.temp_dir, "traces")
        self.output_dir = testsuite.output_dir

        self.source_root = testsuite.env.options.gnatcov_source_root
        """Path of the root of the repository. This will be used with GNATcoverage
        reporting to produce a Cobertura report with relative paths."""

        self.ensure_clean_dir(self.temp_dir)
        os.mkdir(self.traces_dir)

    @staticmethod
    def ensure_clean_dir(dirname):
        """
        If it exists, remove the ``dirname`` directory tree and create an empty
        directory instead.
        """
        if os.path.exists(dirname):
            shutil.rmtree(dirname)
        os.mkdir(dirname)

    @staticmethod
    def checked_run(argv):
        """
        Run a process with the given arguments. Log its output and raise an
        error if it fails.
        """
        p = Run(argv)
        if p.status != 0:
            logging.error(
                "Command failed: %s", " ".join(quote_arg(arg) for arg in argv)
            )
            logging.error("Output:\n" + p.out)
            raise RuntimeError

    def decorate_run(
        self,
        runner: Callable[..., ProcessResult],
        test_driver: TestDriver,
        *args,
        **kwargs
    ) -> ProcessResult:
        """
        Modify e3.os.process.Run arguments to run a test program.

        This must be called for all programs that contribute to code coverage.
        Given a test `driver`, this and return creates a modified copy of
        `kwargs` (the keyword arguments passed to e3.os.process.Run.
        """

        # Unique identifier for the test, used to generate unique file names
        # for traces. This assumes that each testcase runs at most one such
        # program.
        uid = test_driver.test_name
        trace_file = os.path.join(self.traces_dir, uid + ".srctrace")

        env = kwargs.setdefault("env", {})
        env["GNATCOV_TRACE_FILE"] = trace_file

        kwargs.setdefault("ignore_environ", False)

        return runner(*args, **kwargs)

    def report(self, formats=None):
        """
        Generate coverage reports for all given output formats.
        If formats=None, then generates reports with the following formats:
        dhtml, xcov, cobertura, xml
        """

        if formats is None:
            formats = ["dhtml", "xcov", "cobertura", "xml"]

        # Get the list of all SID files
        sid_list = os.path.join(self.temp_dir, "sid_files.txt")
        with open(sid_list, "w") as f:
            for sid_dir in self.sid_dirs:
                for s in glob.glob(os.path.join(sid_dir, "*.sid")):
                    f.write(s + "\n")

        # Get the list of all trace files
        traces_list = os.path.join(self.temp_dir, "traces.txt")
        with open(traces_list, "w") as f:
            for t in glob.glob(os.path.join(self.traces_dir, "*.srctrace")):
                f.write(t + "\n")

        # Load trace files only once, produce a checkpoint for them
        logging.info("Consolidating coverage results")
        ckpt_file = os.path.join(self.temp_dir, "report.ckpt")
        self.checked_run(
            [
                "gnatcov",
                "coverage",
                "--level",
                self.coverage_level,
                "--sid",
                "@" + sid_list,
                "--save-checkpoint",
                ckpt_file,
                "@" + traces_list,
            ]
        )

        # Now, generate all requested reports from this checkpoint
        logging.info(
            "Generating coverage reports ({})".format(", ".join(sorted(formats)))
        )
        for fmt in formats:
            report_dir = os.path.join(self.output_dir, "coverage-" + fmt)
            self.ensure_clean_dir(report_dir)
            cmd = [
                "gnatcov",
                "coverage",
                "--annotate",
                fmt,
                "--level",
                self.coverage_level,
                "--output-dir",
                report_dir,
                "--checkpoint",
                ckpt_file,
            ]

            if fmt == "cobertura" and self.source_root:
                cmd += ["--source-root", self.source_root]

            logging.info(" ".join(cmd))
            self.checked_run(cmd)
