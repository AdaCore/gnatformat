------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                       Copyright (C) 2023, AdaCore                        --
--                                                                          --
-- Libadalang Tools  is free software; you can redistribute it and/or modi- --
-- fy  it  under  terms of the  GNU General Public License  as published by --
-- the Free Software Foundation;  either version 3, or (at your option) any --
-- later version. This software  is distributed in the hope that it will be --
-- useful but  WITHOUT  ANY  WARRANTY; without even the implied warranty of --
-- MERCHANTABILITY  or  FITNESS  FOR A PARTICULAR PURPOSE.                  --
--                                                                          --
-- As a special  exception  under  Section 7  of  GPL  version 3,  you are  --
-- granted additional  permissions described in the  GCC  Runtime  Library  --
-- Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

with Ada.Strings.Unbounded;          use Ada.Strings.Unbounded;

with GNATCOLL.Opt_Parse;             use GNATCOLL.Opt_Parse;
with GNATCOLL.VFS;

with Prettier_Ada.Documents;

--  Gnatformat command line utilities

package Gnatformat.Command_Line is
   Parser : Argument_Parser :=
     Create_Argument_Parser (Help => "GNATformat tool");

   package Config_Filename is new Parse_Positional_Arg
     (Parser   => Parser,
      Name     => "config-file",
      Help     => "Name of the JSON pretty-printer configuration file",
      Arg_Type => Unbounded_String);

   package Project is new Parse_Option
     (Parser      => Parser,
      Short       => "-P",
      Long        => "--project",
      Help        => "Project",
      Arg_Type    => Unbounded_String,
      Convert     => To_Unbounded_String,
      Default_Val => Null_Unbounded_String);

   package Sources is new Parse_Option_List
     (Parser      => Parser,
      Short       => "-S",
      Long        => "--sources",
      Help        => "Source files to format",
      Arg_Type    => Unbounded_String,
      Convert     => To_Unbounded_String);

   package Output_Folder is new Parse_Option
        (Parser      => Parser,
         Short       => "-o",
         Long        => "--output",
         Arg_Type    => Unbounded_String,
         Help        => "Folder to write formatted files",
         Default_Val => Null_Unbounded_String);

   package Width is new Parse_Option
     (Parser      => Parser,
      Short       => "-w",
      Long        => "--width",
      Arg_Type    => Natural,
      Help        => "Maximum line width",
      Default_Val => 79);

   package Indentation_Kind is new Parse_Enum_Option
     (Parser      => Parser,
      Short       => "-k",
      Long        => "--indentation-kind",
      Arg_Type    => Prettier_Ada.Documents.Indentation_Kind,
      Help        => "Indentation kind: spaces or tabs",
      Default_Val => Prettier_Ada.Documents.Spaces);

   package Indentation_Width is new Parse_Option
     (Parser      => Parser,
      Short       => "-i",
      Long        => "--indentation-width",
      Arg_Type    => Natural,
      Help        => "Indentation width",
      Default_Val => 3);

   package Continuation_Indentation_Width is new Parse_Option
     (Parser      => Parser,
      Short       => "-i",
      Long        => "--continuation-indentation-width",
      Arg_Type    => Natural,
      Help        => "Line continuation indentation width",
      Default_Val => 2);

   package End_Of_Line is new Parse_Enum_Option
     (Parser      => Parser,
      Short       => "-e",
      Long        => "--end-of-line",
      Arg_Type    => Prettier_Ada.Documents.End_Of_Line_Kind,
      Help        => "End of line: LF, CR, CRLF",
      Default_Val => Prettier_Ada.Documents.LF);

   package Verbose is new Parse_Flag
     (Parser   => Parser,
      Short    => "-v",
      Long     => "--verbose",
      Help     => "Print traces");

   package Pipe is new Parse_Flag
     (Parser   => Parser,
      Short    => "-p",
      Long     => "--pipe",
      Help     =>
         "Print the result to stdout instead of editing the files on disk");

   package Dump_Document is new Parse_Flag
     (Parser      => Parser,
      Short       => "-d",
      Long        => "--dump-document",
      Help        =>
         "Dump the Prettier document in the corresponding "".json"" file");

   function To_Virtual_File
     (File_Name : String) return GNATCOLL.VFS.Virtual_File
   is (GNATCOLL.VFS.Create (GNATCOLL.VFS."+" (File_Name)));

end Gnatformat.Command_Line;
