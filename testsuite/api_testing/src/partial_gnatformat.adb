--
--  Copyright (C) 2024, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with GNAT.OS_Lib;

with GNATCOLL.Opt_Parse;
with GNATCOLL.Traces;
with GNATCOLL.VFS;

with Libadalang.Analysis; use Libadalang.Analysis;
with Langkit_Support.Slocs;
with Langkit_Support.Diagnostics;
with Langkit_Support.Generic_API.Unparsing;

with Gnatformat;
with Gnatformat.Formatting;
with Gnatformat.Configuration;

--  Testing entry point for the gnatformat library API for IDE integration and
--  partial formatting.

procedure Partial_Gnatformat is

   use type GNATCOLL.VFS.Virtual_File;
   use Langkit_Support.Generic_API.Unparsing;
   use Langkit_Support.Diagnostics;

   -------------------------
   -- Command line parser --
   -------------------------

   package Args is
      use GNATCOLL.Opt_Parse;

      Parser : Argument_Parser := Create_Argument_Parser
        (Help =>
           "Partial_Gnatformat - Format Ada source code (a whole file or "
           & "a selection) and produce formatted edits.");

      function To_Virtual_File
        (File_Name : String) return GNATCOLL.VFS.Virtual_File
      is (GNATCOLL.VFS.Create_From_UTF8 (File_Name));
      --  Creates a file from its display name

      package Unparsing_Config_File is new Parse_Option
        (Parser      => Parser,
         Long        => "--unparsing-config",
         Name        => "UNPARSING_CONFIGURATION_FILE",
         Help        => "Unparsing configuration file",
         Arg_Type    => GNATCOLL.VFS.Virtual_File,
         Convert     => To_Virtual_File,
         Default_Val => GNATCOLL.VFS.No_File);

      package Source_File is new Parse_Positional_Arg
        (Parser      => Parser,
         Name        => "SOURCE FILE",
         Help        => "Source files to format",
         Arg_Type    => Unbounded_String,
         Convert     => To_Unbounded_String);

      --  Selection specifics for partial formatting of a file

      package Selection_Start_Line is new Parse_Option
        (Parser      => Parser,
         Short       => "-SL",
         Long        => "--start-line",
         Help        => "Start line",
         Arg_Type    => Natural,
         Convert     => Natural'Value,
         Default_Val => 0);

      package Selection_Start_Column is new Parse_Option
        (Parser      => Parser,
         Short       => "-SC",
         Long        => "--start-column",
         Help        => "Start column",
         Arg_Type    => Natural,
         Convert     => Natural'Value,
         Default_Val => 0);

      package Selection_End_Line is new Parse_Option
        (Parser      => Parser,
         Short       => "-EL",
         Long        => "--end-line",
         Help        => "End line",
         Arg_Type    => Natural,
         Convert     => Natural'Value,
         Default_Val => 0);

      package Selection_End_Column is new Parse_Option
        (Parser      => Parser,
         Short       => "-EC",
         Long        => "--end-column",
         Help        => "End column",
         Arg_Type    => Natural,
         Convert     => Natural'Value,
         Default_Val => 0);

      package Pipe is new Parse_Flag
        (Parser => Parser,
         Short  => "-p",
         Long   => "--pipe",
         Help   =>
         "Print the result to stdout instead of editing the files on disk");
   end Args;

   Edits : Gnatformat.Formatting.Formatted_Edits;
begin
   GNATCOLL.Traces.Parse_Config_File;

   --  Parse command-line arguments and abort if they are invalid
   if not Args.Parser.Parse then
      GNAT.OS_Lib.OS_Exit (1);
   end if;
   Gnatformat.Gnatformat_Trace.Trace ("Partial_Gnatformat library");

   if To_String (Args.Source_File.Get) = "" then
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error, "A source file must be provided.");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error,
         GNATCOLL.Opt_Parse.Help (Args.Parser));
      GNAT.OS_Lib.OS_Exit (1);
   end if;

   --  Format the given source file or selection and dump the output of
   --  the formatting

   declare
      use Langkit_Support.Slocs;

      Ctx   : constant Analysis_Context := Create_Context;
      Unit  : constant Analysis_Unit    := Ctx.Get_From_File
        (Filename => To_String (Args.Source_File.Get));

      Selection_Range : constant Source_Location_Range :=
        (Line_Number (Args.Selection_Start_Line.Get),
         Line_Number (Args.Selection_End_Line.Get),
         Column_Number (Args.Selection_Start_Column.Get),
         Column_Number (Args.Selection_End_Column.Get));

      use Gnatformat.Configuration;
   begin
      Put_Line (Unit.Root.Image);

      --  Compute the edits for the source file selection. If no selection
      --  range is provided then the whole file will be reformatted.
      if Args.Unparsing_Config_File.Get /= GNATCOLL.VFS.No_File then
         declare
            Unparse_Diag    : Diagnostics_Vectors.Vector;
            Unparsing_Conf  : constant Unparsing_Configuration :=
              Gnatformat.Configuration.Load_Unparsing_Configuration
                (Args.Unparsing_Config_File.Get, Unparse_Diag);
         begin
            Edits := Gnatformat.Formatting.Format_Selection
              (Unit                  => Unit,
               Input_Selection_Range => Selection_Range,
               Options               => Default_Format_Options,
               Unparsing_Config      => Unparsing_Conf);
         end;
      else
         Edits := Gnatformat.Formatting.Format_Selection
           (Unit                  => Unit,
            Input_Selection_Range => Selection_Range,
            Options               => Default_Format_Options);

      end if;

      --  If requested, also dump the reformatted Ada source code to the output
      if Args.Pipe.Get then
         Put_Line (Gnatformat.Formatting.Image (Edits));
         Ada.Text_IO.New_Line;
      end if;
   end;

end Partial_Gnatformat;
