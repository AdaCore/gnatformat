.. include:: ../defines.hrst

Getting Started with |GNATformat|
====================================

Prerequisites
-------------

  * a |GNATPro| installation (the most recent installed on the system) should be
    present in your :envvar:`$PATH`

|GNATformat| setup
---------------------

  * *(on Windows)* run the installer, and place :file:`<install_prefix>\bin` in
    your :envvar:`$PATH`

  * *(on \*NIX)* extract the installation directory from the archive, and place
    :file:`<install_prefix>/bin` in your :envvar:`$PATH`

The 5-lines manual to |GNATformat|
-------------------------------------

|GNATformat| is an opinionated code formatter for the Ada language. It is based
on the |Prettier-Ada|, a port of the |Prettier| formatter engine to the Ada
programming language. These two projects are available on |github| 

  * |Prettier-Ada|: https://github.com/AdaCore/prettier-ada

  * |Prettier|: https://github.com/prettier/prettier

The intent of this tool is to format a valid Ada source code according to the
coding style described in the |GNAT-Style| guide available online at
https://gcc.gnu.org/onlinedocs/gnat-style.pdf 

For more information about the tool, the full manual is available at 

  * :file:`<install_prefix>/share/doc/gnatformat/html`

And online at https://docs.adacore.com/live/wave/gnatformat/html/gnatformat/index.html.


