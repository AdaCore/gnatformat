.. include:: ../defines.hrst

GNATformat and the |GNATcheck| style checks
===========================================

The aim is to list here the |GNATcheck| readability style checks and specify
the way that |GNATformat| is able to automatically address them.  

+-------------------------------------------+----------------------------+
| |GNATcheck| readability style check       | GNATformat positionning    |
+===========================================+============================+
| End_Of_Line_Comments                      | No, comments are preserved |
+-------------------------------------------+----------------------------+
| Headers                                   | Not applicable             |
+-------------------------------------------+----------------------------+
| Identifier_Casing                         | No, casing is preserved    |
+-------------------------------------------+----------------------------+
| Identifier_Prefixes                       | Not applicable             |
+-------------------------------------------+----------------------------+
| Identifier_Suffixes                       | Not applicable             |
+-------------------------------------------+----------------------------+
| Lowercase_Keywords                        | Yes                        |
+-------------------------------------------+----------------------------+
| Max_Identifier_Length                     | Not applicable             |
+-------------------------------------------+----------------------------+
| Min_Identifier_Length                     | Not applicable             |
+-------------------------------------------+----------------------------+
| Misnamed_Controlling_Parameters           | Not applicable             |
+-------------------------------------------+----------------------------+
| Name_Clashes                              | Not applicable             |
+-------------------------------------------+----------------------------+
| No_Dependence                             | Not applicable             |
+-------------------------------------------+----------------------------+
| Numeric_Format                            | No, format is preserved    |
+-------------------------------------------+----------------------------+
| Object_Declarations_Out_Of_Order          | Not applicable             |
+-------------------------------------------+----------------------------+
| One_Construct_Per_Line                    | Yes                        |
+-------------------------------------------+----------------------------+
| Overriding_Indicators                     | Not applicable             |
+-------------------------------------------+----------------------------+
| Profile_Discrepancies                     | Not applicable             |
+-------------------------------------------+----------------------------+
| Style_Checks                              | Yes, |GNAT| style checks   |
+-------------------------------------------+----------------------------+
| Uncommented_BEGIN                         | Not applicable             |
+-------------------------------------------+----------------------------+
| Uncommented_BEGIN_In_Package_Bodies       | Not applicable             |
+-------------------------------------------+----------------------------+
| Uncommented_End_Record                    | Not applicable             |
+-------------------------------------------+----------------------------+


where:

  * `Not applicable` means that |GNATformat| cannot do anything about it,
    
  * `Yes, GNAT style checks` means that |GNATformat| address automatically the
    `-gnaty` checks listed in the `GNATformat and the GNAT style checks (-gnaty)`
    section of the current documentation.
    
    
    
