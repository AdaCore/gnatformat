--
--  Copyright (C) 2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--
--  Package with the public API for editing sources

with Gnatformat.Command_Line;
with Gnatformat.Configuration;

with GPR2.Project.Tree;

with Langkit_Support.Generic_API.Unparsing;

package Gnatformat.Full_Format is

   package Langkit_Support_Unparsing renames
     Langkit_Support.Generic_API.Unparsing;

   procedure Full_Format
     (Project_Tree            : GPR2.Project.Tree.Object;
      CLI_Formatting_Config   : Gnatformat.Configuration.Format_Options_Type;
      Unparsing_Configuration :
        Langkit_Support_Unparsing.Unparsing_Configuration;
      Command_Line_Sources    : Gnatformat.Command_Line.Sources.Result_Array);

end Gnatformat.Full_Format;
