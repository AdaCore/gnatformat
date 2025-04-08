--
--  Copyright (C) 2024-2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;
with GNATCOLL.VFS;

with Gnatformat.Configuration;

with GPR2.Options.Opt_Parse;
--  with Langkit_Support;
--  with Langkit_Support.Slocs;

--  Gnatformat command line utilities

package Gnatformat.Command_Line is

   Parser : Argument_Parser :=
     Create_Argument_Parser
       (Help =>
          "GNATformat "
          & Gnatformat.Version
          & " ("
          & Gnatformat.Build_Date
          & ")"
          & " - Format Ada code");

   function To_Virtual_File
     (File_Name : String) return GNATCOLL.VFS.Virtual_File
   is (GNATCOLL.VFS.Create_From_UTF8 (File_Name));
   --  Creates a file from its display name

   function To_Optional_Positive
     (Number : String) return Gnatformat.Configuration.Optional_Positive
   is ((Is_Set => True, Value => Positive'Value (Number)));
   --  Converts a string to Gnatformat.Configuration.Optional_Positive

   function To_Optional_Indentation_Kind
     (Indentation_Kind : String)
      return Gnatformat.Configuration.Optional_Indentation_Kind
   is ((Is_Set => True,
        Value  =>
          Gnatformat.Configuration.Indentation_Kind'Value (Indentation_Kind)));
   --  Converts a string to Gnatformat.Configuration.Optional_Indentation_Kind

   function To_Optional_End_Of_Line_Kind
     (End_Of_Line_Kind : String)
      return Gnatformat.Configuration.Optional_End_Of_Line_Kind
   is ((Is_Set => True,
        Value  =>
          Gnatformat.Configuration.End_Of_Line_Kind'Value (End_Of_Line_Kind)));
   --  Converts a string to Gnatformat.Configuration.Optional_End_Of_Line_Kind

   function To_Optional_Unbounded_String
     (S : String) return Gnatformat.Configuration.Optional_Unbounded_String
   is ((Is_Set => True, Value => To_Unbounded_String (S)));
   --  Converts a string to Gnatformat.Configuration.Optional_End_Of_Line_Kind

   package Root_Project_Only is new
     Parse_Flag
       (Parser => Parser,
        Long   => "--no-subprojects",
        Help   => "Only process the root project, not the subprojects");

   package All_Sources is new
     Parse_Flag
       (Parser => Parser,
        Short  => "-U",
        Name   => "Process all sources",
        Help   =>
          "Process all files, not only those that are in the closure of mains");

   package Verbose is new
     Parse_Flag
       (Parser => Parser,
        Long   => "--verbose",
        Help   => "Print debug logs");

   package Version is new
     Parse_Flag
       (Parser => Parser,
        Short  => "-v",
        Long   => "--version",
        Help   => "Show the version");

   package Check is new
     Parse_Flag
       (Parser => Parser,
        Long   => "--check",
        Help   =>
          "Exit with error code 1 if the input is not formatted correctly and "
          & "prints the file names that would be formatted");

   package Pipe is new
     Parse_Flag
       (Parser => Parser,
        Short  => "-p",
        Long   => "--pipe",
        Help   =>
          "Print the result to stdout instead of editing the files on disk "
          & "(takes precedence over --check)");

   package Keep_Going is new
     Parse_Flag
       (Parser => Parser,
        Short  => "-k",
        Long   => "--keep-going",
        Help   => "Keep going after errors formatting a source file");

   package Unparsing_Configuration is new
     Parse_Option
       (Parser      => Parser,
        Long        => "--unparsing-configuration",
        Name        => "UNPARSING_CONFIGURATION_FILE",
        Help        => "Unparsing configuration file",
        Arg_Type    => GNATCOLL.VFS.Virtual_File,
        Convert     => To_Virtual_File,
        Default_Val => GNATCOLL.VFS.No_File);

   package Width is new
     Parse_Option
       (Parser      => Parser,
        Short       => "-w",
        Long        => "--width",
        Help        => "Max line width (default value = 79)",
        Arg_Type    => Gnatformat.Configuration.Optional_Positive,
        Convert     => To_Optional_Positive,
        Default_Val => Gnatformat.Configuration.Optional_Positives.None);

   package Indentation is new
     Parse_Option
       (Parser      => Parser,
        Short       => "-i",
        Long        => "--indentation",
        Help        => "Indentation size (default value = 3)",
        Arg_Type    => Gnatformat.Configuration.Optional_Positive,
        Convert     => To_Optional_Positive,
        Default_Val => Gnatformat.Configuration.Optional_Positives.None);

   package Indentation_Kind is new
     Parse_Option
       (Parser      => Parser,
        Long        => "--indentation-kind",
        Help        =>
          "Indentation kind: tabs | spaces (default value = spaces)",
        Arg_Type    => Gnatformat.Configuration.Optional_Indentation_Kind,
        Convert     => To_Optional_Indentation_Kind,
        Default_Val =>
          Gnatformat.Configuration.Optional_Indentation_Kinds.None);

   package Indentation_Continuation is new
     Parse_Option
       (Parser      => Parser,
        Long        => "--indentation-continuation",
        Help        =>
          "Continuation Line Indentation size (default value = indentation - 1)",
        Arg_Type    => Gnatformat.Configuration.Optional_Positive,
        Convert     => To_Optional_Positive,
        Default_Val => Gnatformat.Configuration.Optional_Positives.None);

   package End_Of_Line is new
     Parse_Option
       (Parser      => Parser,
        Long        => "--end-of-line",
        Help        => "End of line sequence: lf | crlf (default value = lf)",
        Arg_Type    => Gnatformat.Configuration.Optional_End_Of_Line_Kind,
        Convert     => To_Optional_End_Of_Line_Kind,
        Default_Val =>
          Gnatformat.Configuration.Optional_End_Of_Line_Kinds.None);

   package Charset is new
     Parse_Option
       (Parser      => Parser,
        Long        => "--charset",
        Help        =>
          "Charset to use for source decoding (default value = iso-8859-1)",
        Arg_Type    => Gnatformat.Configuration.Optional_Unbounded_String,
        Convert     => To_Optional_Unbounded_String,
        Default_Val =>
          Gnatformat.Configuration.Optional_Unbounded_Strings.None);

   package Ignore is new
     Parse_Option
       (Parser      => Parser,
        Long        => "--ignore",
        Help        => "Ignore file with the sources that will be ignored",
        Arg_Type    => Gnatformat.Configuration.Optional_Unbounded_String,
        Convert     => To_Optional_Unbounded_String,
        Default_Val =>
          Gnatformat.Configuration.Optional_Unbounded_Strings.None);

   package Gitdiff is new
     Parse_Option
       (Parser      => Parser,
        Long        => "--gitdiff",
        Help        => "Only format lines added since given commit",
        Arg_Type    => Gnatformat.Configuration.Optional_Unbounded_String,
        Convert     => To_Optional_Unbounded_String,
        Default_Val =>
          Gnatformat.Configuration.Optional_Unbounded_Strings.None,
        Usage_Text  => "[--gitdiff BASE_COMMIT]");

   package Sources is new
     Parse_Positional_Arg_List
       (Parser      => Parser,
        Name        => "SOURCE",
        Allow_Empty => True,
        Arg_Type    => GNATCOLL.VFS.Virtual_File,
        Convert     => To_Virtual_File,
        Help        => "Source files to format");

   --  Range formatting relocated here with the specific switches
   package Range_Format is new
     Parse_Flag
       (Parser => Parser,
        Long   => "--range-format",
        Help   => "Selection formatting mode activated");

   package Start_Line is new
     Parse_Option
        (Parser      => Parser,
         Short       => "-SL",
         Long        => "--start-line",
         Help        =>
            "Selection start line number for selection formatting mode",
         Arg_Type    => Natural,
         Convert     => Natural'Value,
         Default_Val => 0);

   package Start_Column is new
     Parse_Option
        (Parser      => Parser,
         Short       => "-SC",
         Long        => "--start-column",
         Help        =>
            "Selection start column number for selection formatting mode",
         Arg_Type    => Natural,
         Convert     => Natural'Value,
         Default_Val => 0);

   package End_Line is new
     Parse_Option
        (Parser      => Parser,
         Short       => "-EL",
         Long        => "--end-line",
         Help        =>
            "Selection end line number for selection formatting mode",
         Arg_Type    => Natural,
         Convert     => Natural'Value,
         Default_Val => 0);

   package End_Column is new
     Parse_Option
        (Parser      => Parser,
         Short       => "-EC",
         Long        => "--end-column",
         Help        =>
            "Selection end column number for selection formatting mode",
         Arg_Type    => Natural,
         Convert     => Natural'Value,
         Default_Val => 0);

   package GPR_Args is new GPR2.Options.Opt_Parse.Args (Parser);

end Gnatformat.Command_Line;
