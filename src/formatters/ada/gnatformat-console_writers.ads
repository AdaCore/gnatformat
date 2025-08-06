--
--  Copyright (C) 2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--
--  The package provides a writer to emit formatted sources, errors and
--  warnings on stdout/stderr.

with Ada.Strings.Unbounded;

with Gnatformat.Abstract_Writers;

package Gnatformat.Console_Writers is

   type Console_Writer is limited
     new Gnatformat.Abstract_Writers.Abstract_Writer with private;

   function Writer (Single_File : Boolean) return Console_Writer;

private

   type Console_Writer is limited
     new Gnatformat.Abstract_Writers.Abstract_Writer
   with record
      Single_File    : Boolean;
      Print_New_Line : Boolean := False;
   end record;

   overriding
   procedure Print_Error
     (Self       : in out Console_Writer;
      Line       : String;
      Extra_Line : Boolean := True);

   overriding
   procedure Print_Source
     (Self   : in out Console_Writer;
      Ignore : String;
      Text   : Ada.Strings.Unbounded.Unbounded_String);

   overriding
   procedure Print_Source_Name (Self : in out Console_Writer; Line : String);

   overriding
   procedure Print_Warning (Self : in out Console_Writer; Line : String);

   function Writer (Single_File : Boolean) return Console_Writer
   is (Gnatformat.Abstract_Writers.Abstract_Writer
       with Single_File => Single_File, Print_New_Line => False);

end Gnatformat.Console_Writers;
