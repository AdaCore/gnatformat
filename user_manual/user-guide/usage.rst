.. include:: ../defines.hrst

How to use |GNATformat|
=======================

|GNATformat| is available as a command line tool or as a library.

As a command line tool
----------------------

The ``gnatformat`` command line works as other |GNAT| tools by providing
|GNAT| project file (:file:`.gpr`)::

    gnatformat -P [project_name].gpr

For the given project, this command will format the files from all closures
of mains or library entry points, recursing on all the subprojects and
stopping when a project is marked as ``externally built``.

In order to customize your formatting, a few options are available and can
be listed by executing the ``gnatformat --help`` command.

The available global options are:

* ``--help, -h``: print a help message and exit.
* ``--project, -P``: specify the project file to load; the .gpr extension can be omitted
  if the file is in the current directory
* ``-X``: allows to specify an external reference to a scenario variable.
* ``--no-subprojects``: only process the root project, not the subprojects.
* ``-U``: process all files, not only those that are in the closure of mains.
* ``--verbose``: prints additional logs.
* ``--version, -v``: shows the version of the tool.  
* ``--check``: exit with error code 1 if the input is not formatted correctly and
  print the name of files that would be formatted.  
* ``--pipe, -p``: print the result to stdout instead of editing the files on disk.  
* ``--keep-going, -k``: print the result to stdout instead of editing the files on disk.

The specific options allowing to customize the formatting of your sources are:

* ``--unparsing-configuration``: allows to specify a custom unparsing configuration file
  to be used instead of the default one.
* ``--width``: allows to specify the maximum line length.
  In the absence of this, the default value is ``79``.
* ``--indentation``: allows to specify the indentation size.
  In the absence of this, the default value is ``3``.
* ``--indentation-kind``: allows to specify the indentation kind (i.e., ``tabs`` or ``spaces``).
  In the absence of this, the choice by default is ``spaces``.
* ``--indentation-continuation``: allows to specify the continuation line indentation size.
  In the absence of this, the default value is ``indentation - 1``.
* ``--end-of-line``: allows you to choose the end of line sequence (i.e., ``lf`` or ``crlf``).
  In the absence of this, the default value is ``lf``.

As a libray
-----------

The formatting functionality is also available as via a library.

This ``gnatformat`` library is included in the |ALS| and can therefore be used through IDEs
like |GNATStudio| and |VSCode|. 


TO BE COMPLETED!!!
