--
--  Copyright (C) 2025-2026, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--
--  Package with the public API for editing sources

with GNATCOLL.VFS;

with Gnatformat.Configuration;

with GPR2.Project.Tree;

with Langkit_Support.Slocs;

package Gnatformat.Range_Format is

   procedure Range_Format
     (Project_Tree                 : GPR2.Project.Tree.Object;
      Source                       : GNATCOLL.VFS.Virtual_File;
      Selection_Range              :
        Langkit_Support.Slocs.Source_Location_Range;
      CLI_Formatting_Config        :
        Gnatformat.Configuration.Format_Options_Type;
      Unparsing_Configuration_File : GNATCOLL.VFS.Virtual_File :=
        GNATCOLL.VFS.No_File;
      Default_Charset              : String :=
        Gnatformat.Configuration.Default_Charset;
      Pipe                         : Boolean := False);
   --  Range formats the Selection_Range of Source.

end Gnatformat.Range_Format;
