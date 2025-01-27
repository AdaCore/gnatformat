.. include:: ../defines.hrst

Configuring |GNATformat|
========================

|GNATformat| provides few configurable options that can be provided:

* as command line arguments,
* as project attributes defined at the :file:`.gpr` level.


The command line arguments
--------------------------

The formatting of your sources can be customized by the following options:

* ``--width`` to be used if the maximum line length in your sources needs to be specified.
  If not defined, the default value is set to ``79``.
* ``--indentation`` is an option allowing to specify the line's indentation size if needed.
  By default its value is set to ``3``.
* ``--indentation-kind`` is an option that can be used to specify the indentation kind.
  The possible values are ``tabs`` or ``spaces``. By default, this value is set to ``spaces``.
* ``--indentation-continuation`` is an option allowing to specify the continuation line
  indentation size in case of your source code line should break relatively to the previous line.
  By default this value is set to ``indentation - 1``.
* ``--end-of-line`` is an option allowing to choose the end of line sequence in your file
  (i.e., ``lf`` or ``crlf``). By default, this value is set to ``lf``.
* ``--charset`` is an option allowing to specify the charset to use for the sources decoding.

The tool allows as well the usage of a custom unparsing configuration file. This file can be
specified instead of the default one using the ``--unparsing-configuration`` switch taking as
argument the custom configuration file name. However, for the moment this is only an internal
switch and its usage is limited for development proposes.


The project file attributes
---------------------------

The formatting of your sources can be customized through the :file:`.gpr` project file by defining
a specific package called ``package Format``.

In this package all the attributes corresponding to the command line arguments can be added. There is
a one to one correspondence between the command line arguments and project file attributes.

The attribute has the same functionality as its associated command line argument and can be customized
in order to comply with a specific source code formatting use case.

The lines below shows the implementation of the ``Format`` package as part of the project file :file:`.gpr`::

  package Format is

    for Indentation ("Ada") use "3"; -- this is the default

    for Indentation ("some_source.ads") use "4";

    for Indentation_Kind ("Ada") use "spaces"; -- this is the default

    for Indentation_Kind ("some_source.ads") use "tabs";

    for Width ("Ada") use "79"; -- this is the default

    for Width ("some_source.ads") use "99";

    for End_Of_Line ("Ada") use "lf"; -- this is the default

    for End_Of_Line ("some_source.ads") use "crlf";

    for Charset ("Ada") use "iso-8869-1"; -- this is the default

    for Charset ("some_source.ads") use "utf-8";

  end Format;


Preprocessing
-------------

The current support for preprocessing is minimal. Sources that require preprocessing are skipped.

GNATformat automatically detects the ``-gnatep`` and ``-gnateD`` switches present in the project
file, however, the ``-gnateD`` switch is not supported.

Sources found in the preprocessor data file provided by the ``-gnatep`` switch are skipped. Symbols
provided by the ``-gnateD`` switch are applied globally and currently GNATformat is not able to
detect which sources need preprocessing apriori, therefore, the switch is not supported. As a
workaround, use a preprocessor data file with the ``-gnatep`` switch.


Formatting Control Regions
--------------------------

GNATformat allows the user to specify regions of the source code which should not be formatted.
These regions are delimited by the following pairs of whole line comments:

* ``--format off`` / ``--format on``
* ``--begin read only`` / ``--end read only``, for GNATtest users
* ``--pp off`` / ``--pp on``, for GNATpp users

Additionally, the user is allowed to specify just the "off" comment (e.g., ``--format off``), in
which case the rest of the file will not be formatted.
