.. include:: ../defines.hrst

GNATformat and the |GNAT| style checks (-gnaty)
===============================================

The |GNATformat| intent is to format a valid  Ada source code according to
the coding style described in the |GNAT-Style| guide available online at
https://gcc.gnu.org/onlinedocs/gnat-style.pdf 

The aim of this section is to list the |GNAT| style checks options and specify
the way that |GNATformat| is able to automatically address them.  


+---------------+------------------------------------------+-----------------------------+
| |GNAT| option | Option description                       | GNATformat positionning     |
+===============+==========================================+=============================+
|    -gnaty0    | Specify indentation level                | Yes, allows to control      |
|               |                                          | the indentation level       |
+---------------+------------------------------------------+-----------------------------+
|    -gnatya    | Check attribute casing                   | No, the casing is preserved |
+---------------+------------------------------------------+-----------------------------+
|    -gnatyA    | Use of array index numbers in array      | Not applicable              |
|               | attributes                               |                             |
+---------------+------------------------------------------+-----------------------------+
|    -gnatyb    | Blanks not allowed at statement end      | Yes, the trailing blanks    |
|               |                                          | are removed                 |
+---------------+------------------------------------------+-----------------------------+
|    -gnatyB    | Check Boolean operators                  | Not applicable              |
+---------------+------------------------------------------+-----------------------------+
|    -gnatyc    | Check comments, double space             | No, the comments are        |
|               |                                          | preserved and they will     |
|               |                                          | be indented when needed     |
+---------------+------------------------------------------+-----------------------------+
|    -gnatyC    | Check comments, single space             | No, the comments are        |
|               |                                          | preserved and they will     |
|               |                                          | be indented when needed     |
+---------------+------------------------------------------+-----------------------------+
|    -gnatyd    | Check no DOS line terminators present    | Yes, allows to specify      |
|               |                                          | the EOL character           |
+---------------+------------------------------------------+-----------------------------+
|    -gnatyD    | Check declared identifiers in mixed case | No, the casing is preserved |
+---------------+------------------------------------------+-----------------------------+
|    -gnatye    | Check end or exit labels                 | Not applicable              |
+---------------+------------------------------------------+-----------------------------+
|    -gnatyf    | No form feeds or vertical tabs           | Yes, not used to generate   |
|               |                                          | the formatted sources       |
+---------------+------------------------------------------+-----------------------------+
|    -gnatyg    | GNAT style mode                          | Partially, check the        |
|               |                                          | individual switches         |
+---------------+------------------------------------------+-----------------------------+
|    -gnatyh    | No horizontal tabs                       | Yes, allows to specify if   |
|               |                                          | tabs or spaces are used     |
+---------------+------------------------------------------+-----------------------------+
|    -gnatyi    | Check if-then layout                     | Yes                         |
+---------------+------------------------------------------+-----------------------------+
|    -gnatyI    | Check mode IN keywords                   | Not applicable              |
+---------------+------------------------------------------+-----------------------------+
|    -gnatyk    | Check keyword casing                     | No, the casing is preserved |
+---------------+------------------------------------------+-----------------------------+
|    -gnatyl    | Check layout                             | Yes                         |
+---------------+------------------------------------------+-----------------------------+
|    -gnatyL    | Set maximum nesting level                | Not applicable              |
+---------------+------------------------------------------+-----------------------------+
|    -gnatym    | Check maximum line length                | Yes, the line length can    |
|               |                                          | be customized               |
+---------------+------------------------------------------+-----------------------------+
|    -gnatyM    | Set maximum line length                  | Yes                         |
+---------------+------------------------------------------+-----------------------------+
|    -gnatyn    | Check casing of entities in Standard     | No, the casing is preserved |
+---------------+------------------------------------------+-----------------------------+
|    -gnatyN    | Turn off all style checks                | Not applicable              |
+---------------+------------------------------------------+-----------------------------+
|    -gnatyo    | Check order of subprogram bodies         | Not applicable              |
+---------------+------------------------------------------+-----------------------------+
|    -gnatyO    | Check that overriding subprograms are    | Not applicable              |      
|               | explicitly marked as such                |                             |
+---------------+------------------------------------------+-----------------------------+
|    -gnatyp    | Check pragma casing                      | No, the casing is preserved |
+---------------+------------------------------------------+-----------------------------+
|    -gnatyr    | Check references                         | No, the casing is preserved |
+---------------+------------------------------------------+-----------------------------+
|    -gnatys    | Check separate specs                     | Not applicable              |
+---------------+------------------------------------------+-----------------------------+
|    -gnatyS    | Check no statements after then/else      | Yes                         |
+---------------+------------------------------------------+-----------------------------+
|    -gnatyt    | Check token spacing                      | Yes                         |
+---------------+------------------------------------------+-----------------------------+
|    -gnatyu    | Check unnecessary blank lines            | Yes                         |
+---------------+------------------------------------------+-----------------------------+
|    -gnatyx    | Check extra parentheses                  | Not applicable              |
+---------------+------------------------------------------+-----------------------------+
|    -gnatyy    | Set all standard style check options     | Partially, check the        |
|               |                                          | individual switches         |
+---------------+------------------------------------------+-----------------------------+
|    -gnatyz    | Check extra parentheses (operator        | Not applicable              |
|               | precedence)                              |                             |
+---------------+------------------------------------------+-----------------------------+
|    -gnaty-    | Remove style check options               | Yes, with --!format off     |
+---------------+------------------------------------------+-----------------------------+
|    -gnaty+    | Enable style check options               | Yes, with --!format on      |
+---------------+------------------------------------------+-----------------------------+


where 

  * `Not applicable` means that |GNATformat| cannot do anything about it.
