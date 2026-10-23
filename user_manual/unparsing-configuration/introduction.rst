.. include:: ../defines.hrst

The configurability of |GNATformat| 
===================================

|GNATformat| is an opinionated code formatter for the Ada language. It is based
on the |Prettier-Ada|, a port of the |Prettier| formatter engine to the Ada
programming language. 

The formatting layout is managed through a JSON file that defines document templates,
which are patterns used to generate Prettier documents. 

It is possible to customize the built-in layouts by:

* defining custom configuration snippets for the specific nodes you want to change
* passing them to overwrite those nodes in the active layout as the ``--override-layout``
  switch argument.

If a configuration cannot be loaded, |GNATformat| reports the configuration
paths and the unparsing engine's diagnostics on standard error and exits with
status 1. This applies to ``--unparsing-configuration``, layout overrides,
and the ``Format.Override_Layout`` project attribute, including range formatting.
An explicitly specified configuration that does not exist is an error;
|GNATformat| uses the default configuration only when no custom base
configuration was specified. With ``--keep-going``, the diagnostics are reported
for each affected source, including sources that share a cached configuration.

To help to define custom formatting layouts, the next section covers the document
template JSON file structure and the available commands, followed by practical
configuration snippet examples.
