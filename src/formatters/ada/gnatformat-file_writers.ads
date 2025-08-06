--
--  Copyright (C) 2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--
--  The package provides a writer to emit formatted sources to a file and
--  errors/warnings to stdout/stderr.

with Ada.Strings.Unbounded;

with Gnatformat.Abstract_Writers;
with Gnatformat.Console_Writers;

package Gnatformat.File_Writers is

   type File_Writer is limited
     new Gnatformat.Abstract_Writers.Abstract_Writer with private;

   function Writer return File_Writer;

private

   type File_Writer is limited new Gnatformat.Console_Writers.Console_Writer
   with null record;

   overriding
   procedure Print_Source
     (Self : in out File_Writer;
      File : String;
      Text : Ada.Strings.Unbounded.Unbounded_String);

   overriding
   procedure Print_Source_Name (Self : in out File_Writer; Line : String)
   is null;

   function Writer return File_Writer
   is (Gnatformat.Console_Writers.Writer (False) with null record);

end Gnatformat.File_Writers;
