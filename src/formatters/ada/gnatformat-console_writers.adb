--
--  Copyright (C) 2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Text_IO;
with Ada.Strings.Unbounded.Text_IO;

package body Gnatformat.Console_Writers is

   -----------------
   -- Print_Error --
   -----------------

   overriding
   procedure Print_Error
     (Self       : in out Console_Writer;
      Line       : String;
      Extra_Line : Boolean := True) is
   begin
      if Extra_Line then
         if Self.Print_New_Line then
            Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
         else
            Self.Print_New_Line := True;
         end if;
      end if;

      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, Line);
   end Print_Error;

   ------------------
   -- Print_Source --
   ------------------

   overriding
   procedure Print_Source
     (Self   : in out Console_Writer;
      Ignore : String;
      Text   : Ada.Strings.Unbounded.Unbounded_String)
   is
      pragma Unreferenced (Self);
   begin
      Ada.Strings.Unbounded.Text_IO.Put (Text);
   end Print_Source;

   -----------------------
   -- Print_Source_Name --
   -----------------------

   overriding
   procedure Print_Source_Name (Self : in out Console_Writer; Line : String) is
   begin
      if not Self.Single_File then
         if Self.Print_New_Line then
            Ada.Text_IO.New_Line;
         else
            Self.Print_New_Line := True;
         end if;

         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, Line);
      end if;
   end Print_Source_Name;

   -------------------
   -- Print_Warning --
   -------------------

   overriding
   procedure Print_Warning (Self : in out Console_Writer; Line : String) is
   begin
      if Self.Print_New_Line then
         Ada.Text_IO.New_Line;
      else
         Self.Print_New_Line := True;
      end if;

      Ada.Text_IO.Put_Line (Line);
   end Print_Warning;

end Gnatformat.Console_Writers;
