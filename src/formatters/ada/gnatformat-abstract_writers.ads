--
--  Copyright (C) 2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--
--  This package defines abstract writer interface and corresponding operations
--  to emit formatted sources, errors and warnings.

with Ada.Strings.Unbounded;

package Gnatformat.Abstract_Writers is

   type Abstract_Writer is limited interface;

   procedure Print_Source
     (Self : in out Abstract_Writer;
      File : String;
      Text : Ada.Strings.Unbounded.Unbounded_String)
   is abstract;

   procedure Print_Warning (Self : in out Abstract_Writer; Line : String)
   is abstract;
   --  Print Line on stdout. Ensure that Line is printed on a separate line.

   procedure Print_Error
     (Self       : in out Abstract_Writer;
      Line       : String;
      Extra_Line : Boolean := True)
   is abstract;
   --  Print Line on stderr. If Extra_Line=True then put a new-line before

   procedure Print_Source_Name (Self : in out Abstract_Writer; Line : String)
   is abstract;
   --  Do nothing if we have only single source file, otherwise
   --  print Line on stdout. Ensure that Line is printed on a separate line.

end Gnatformat.Abstract_Writers;
