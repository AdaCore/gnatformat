--
--  Copyright (C) 2024, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Gnatformat.Abstract_Writers;
with Gnatformat.Configuration;

with Langkit_Support.Generic_API.Unparsing;

package Gnatformat.Command_Line.Configuration is

   function Get return Gnatformat.Configuration.Format_Options_Type;
   --  Builds a Format_Options_Type from the command line arguments

   function Get_Unparsing_Configuration
     (Cache           :
        in out Gnatformat.Configuration.Unparsing_Configuration_Cache_Type;
      Source_Filename : String;
      Format_Options  : Gnatformat.Configuration.Format_Options_Type;
      Writer          : in out Abstract_Writers.Abstract_Writer'Class)
      return Langkit_Support.Generic_API.Unparsing.Unparsing_Configuration;
   --  Load the source's configuration and report all loading diagnostics.
   --  On failure, mark the command as failed and return
   --  No_Unparsing_Configuration. Callers must not format with that value.
   --  Source_Filename may be empty when loading the shared configuration
   --  before discovering the sources to process in git-diff mode.

end Gnatformat.Command_Line.Configuration;
