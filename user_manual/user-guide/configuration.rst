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
* ``--keyword-casing``: allows to choose the Ada language keyword casing through the options:
  ``keep``  - which preserves the current casing of keywords,
  ``lower`` - which convert all the keywords in lower case  and
  ``upper`` - which converst all the keywords in upper case.
  By default this option is set to ``keep``.
* ``--identifier-casing``: allows to fix the casing of identifiers through the options:
  ``keep`` - which preserves the current casing of identifiers,
  ``definition`` - which rewrites every identifier occurrence (references, ``end`` labels and
  declarations) to match the casing of its declaration, resolved with Libadalang, and
  ``lower`` (``my_var``), ``upper`` (``MY_VAR``) and ``mixed`` (``My_Var``) - which recase
  every identifier lexically, without requiring name resolution.
  For ``definition``, cross-unit references are resolved when a project is provided (with ``-P``);
  otherwise only references whose declaration is in the same source file are fixed. Identifiers
  inside formatting-off regions (e.g. ``--!format off``) are left untouched. In range-formatting
  mode (``--range-format``) only the identifiers within the selected region are recased. By default
  this option is set to ``keep``.
* ``--layout``: allows to choose one of the builtin layouts (i.e., ``default`` or ``tall``).
  By default is set to ``default``.
* ``--override-layout``: allows to define the usage of custom configurations for specific nodes. 
  Pass your custom configuration files (:file:`.json`) after this switch to rewrite the nodes 
  using your specified formatting preferences.
* ``--end-of-line`` is an option allowing to choose the end of line sequence in your file
  (i.e., ``lf`` or ``crlf``). By default, this value is set to ``lf``.
* ``--charset`` is an option allowing to specify the charset to use for the sources decoding.
  The formatted sources are encoded back using the same charset, so the input encoding is
  preserved. When neither this option nor the project's ``Charset`` attribute is set, the
  encoding of each source is detected: a byte order mark decides it, otherwise sources
  with non-ASCII contents that are valid UTF-8 are treated as ``utf-8``, and anything
  else falls back to ``iso-8859-1`` when plausible. Sources whose encoding cannot be
  determined are skipped with a warning: files containing NUL bytes (which suggests
  UTF-16 or UTF-32 without a byte order mark) or bytes that never appear in ISO 8859
  text (C1 control characters, typically Windows-1252 punctuation). In range formatting
  mode such a source is reported as an error instead. When a charset is configured and the
  source starts with a byte order mark compatible with it (``utf-8`` for a UTF-8 byte order
  mark, ``utf-16`` or ``utf-16le`` for a UTF-16 little endian one, and so on), the byte order
  mark is honoured and preserved. A source starting with any other byte order mark (e.g. a
  UTF-16 little endian one with ``utf-16be``, or a UTF-8 one with ``iso-8859-1``) fails to
  format with an error naming the byte order mark and the configured charset, since that
  charset cannot be right for it. The ``--gitdiff`` mode also skips sources that start with
  a byte order mark. In range formatting mode, the text of the printed edit is encoded in
  the source's charset, like the rest of the formatted output.
* ``--ignore`` is an option allowing to specify a file with the source file names that must not be
  formatted.
* ``--gitdiff`` is an option to format only the lines added since a given commit.
* ``--range-format`` is an option allowing to enter in selection range formatting mode.
* ``--start-line, -SL`` is an option allowing to specify the selection's start line number for
  range formatting.
* ``--start-column, -SC`` is an option allowing to specify the selection's start column number for
  range formatting.
* ``--end-line, -EL`` is an option allowing to specify the selection's end line number for range
  formatting.
* ``--end-column, -EC`` is an option allowing to specify the selection's end column number for
  range formatting.

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

    for Keyword_Casing ("Ada") use "keep"; -- this is the default

    for Keyword_Casing ("some_source.ads") use "lower";

    for Identifier_Casing ("Ada") use "keep"; -- this is the default

    for Identifier_Casing ("some_source.ads") use "definition";

    for Layout ("Ada") use "default"; -- this is the default

    for Layout ("some_source.ads") use "tall";
    
    for Override_Layout ("Ada") use ("custom_config.json", other_custom_config.json);

    for Override_Layout ("some_source.ads") use ("custom_config.json");

    for Width ("Ada") use "79"; -- this is the default

    for Width ("some_source.ads") use "99";

    for End_Of_Line ("Ada") use "lf"; -- this is the default

    for End_Of_Line ("some_source.ads") use "crlf";

    for Charset ("Ada") use "iso-8859-1"; -- by default, the charset is detected per source

    for Charset ("some_source.ads") use "utf-8";

    for Ignore use "ignore.txt"; -- by default this attribute has no value

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

* ``--!format off`` / ``--!format on``
* ``--  begin read only`` / ``--  end read only``, for GNATtest users
* ``--!pp off`` / ``--!pp on``, for GNATpp users

Additionally, the user is allowed to specify just the "off" comment (e.g., ``--!format off``), in
which case the rest of the file will not be formatted.
