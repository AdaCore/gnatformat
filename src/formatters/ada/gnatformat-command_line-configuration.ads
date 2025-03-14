--
--  Copyright (C) 2024, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Gnatformat.Configuration;

package Gnatformat.Command_Line.Configuration is

   function Get return Gnatformat.Configuration.Format_Options_Type;
   --  Builds a Format_Options_Type from the command line arguments

end Gnatformat.Command_Line.Configuration;
