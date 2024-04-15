--
--  Copyright (C) 2024, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;
with GNATCOLL.VFS;

--  Gnatformat command line utilities

package Gnatformat.Command_Line is

   Parser : Argument_Parser :=
     Create_Argument_Parser
       (Help => "GNATformat " & Gnatformat.Version & " - Format Ada code");

   function To_Virtual_File
     (File_Name : String) return GNATCOLL.VFS.Virtual_File
   is (GNATCOLL.VFS.Create (GNATCOLL.VFS."+" (File_Name)));

   package Project is new Parse_Option
     (Parser      => Parser,
      Short       => "-P",
      Long        => "--project",
      Help        =>
        "Specify the project file to load; the .gpr extension can be omitted "
        & " if the file is in the current directory",
      Arg_Type    => GNATCOLL.VFS.Virtual_File,
      Convert     => To_Virtual_File,
      Default_Val => GNATCOLL.VFS.No_File);

   package Scenario is new Parse_Option_List
     (Parser   => Parser,
      Long     => "-X",
      Name     => "KEY=VALUE",
      Help     =>
        "Specify an external reference to a scenario variable",
      Arg_Type => Unbounded_String,
      Convert  => To_Unbounded_String);

   package No_Subprojects is new Parse_Flag
      (Parser => Parser,
       Long   => "--no-subprojects",
       Help   => "Only process the root project, not the subprojects");

   package Process_All_Files is new Parse_Flag
     (Parser => Parser,
      Short  => "-U",
      Name   => "Process All Files",
      Help   =>
        "Process all files, not only those that are in the closure of mains");

   package Verbose is new Parse_Flag
     (Parser => Parser,
      Long   => "--verbose",
      Help   => "Print debug logs");

   package Version is new Parse_Flag
     (Parser => Parser,
      Short  => "-v",
      Long   => "--version",
      Help   => "Show the version");

   package Check is new Parse_Flag
     (Parser => Parser,
      Long   => "--check",
      Help   =>
        "Exit with error code 1 if the input is not formatted correctly; "
        & "print the name of files that would be formatted");

   package Pipe is new Parse_Flag
     (Parser => Parser,
      Short  => "-p",
      Long   => "--pipe",
      Help   =>
        "Print the result to stdout instead of editing the files on disk");

   package Config is new Parse_Option_List
     (Parser => Parser,
      Long   => "--config",
      Help   =>
        "Formatting settings; "
        & "these take priority over settings defined in the project file",
      Arg_Type => Unbounded_String,
      Convert  => To_Unbounded_String);

   package Rules is new Parse_Option
     (Parser      => Parser,
      Long        => "--rules",
      Name        => "RULES_FILE",
      Help        => "Formatting rules file",
      Arg_Type    => GNATCOLL.VFS.Virtual_File,
      Convert     => To_Virtual_File,
      Default_Val => GNATCOLL.VFS.No_File);

   package Sources is new Parse_Positional_Arg_List
     (Parser      => Parser,
      Name        => "SOURCE",
      Allow_Empty => True,
      Arg_Type    => Unbounded_String,
      Convert     => To_Unbounded_String,
      Help        => "Source files to format");

end Gnatformat.Command_Line;
