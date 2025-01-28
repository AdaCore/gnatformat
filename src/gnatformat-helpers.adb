--
--  Copyright (C) 2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Streams;
with Ada.Streams.Stream_IO;
pragma Warnings (Off, "-gnatwi");
with Ada.Strings.Unbounded.Aux;
pragma Warnings (On, "-gnatwi");

package body Gnatformat.Helpers is

   ---------------
   -- Read_File --
   ---------------

   function Read_To_Unbounded_String
     (Path : String) return Ada.Strings.Unbounded.Unbounded_String
   is
      Result : Ada.Strings.Unbounded.Unbounded_String :=
        Ada.Strings.Unbounded.Null_Unbounded_String;

      File   : Ada.Streams.Stream_IO.File_Type;
      Buffer : Ada.Streams.Stream_Element_Array (1 .. 1024);
      Last   : Ada.Streams.Stream_Element_Offset;

   begin
      Ada.Streams.Stream_IO.Open
        (File => File, Mode => Ada.Streams.Stream_IO.In_File, Name => Path);

      while not Ada.Streams.Stream_IO.End_Of_File (File) loop
         Ada.Streams.Stream_IO.Read (File, Buffer, Last);
         for I in 1 .. Last loop
            Ada.Strings.Unbounded.Append (Result, Character'Val (Buffer (I)));
         end loop;
      end loop;

      Ada.Streams.Stream_IO.Close (File);

      return Result;
   end Read_To_Unbounded_String;

   -----------
   -- Write --
   -----------

   procedure Write
     (Path : String; Contents : Ada.Strings.Unbounded.Unbounded_String)
   is
      Constents_Access : Ada.Strings.Unbounded.Aux.Big_String_Access;
      Constents_Length : Natural;

      Output_File   : Ada.Streams.Stream_IO.File_Type;
      Output_Stream : Ada.Streams.Stream_IO.Stream_Access;

   begin
      Ada.Strings.Unbounded.Aux.Get_String
        (Contents, Constents_Access, Constents_Length);

      Ada.Streams.Stream_IO.Create
        (File => Output_File,
         Mode => Ada.Streams.Stream_IO.Out_File,
         Name => Path);

      Output_Stream := Ada.Streams.Stream_IO.Stream (Output_File);

      String'Write
        (Output_Stream, Constents_Access.all (1 .. Constents_Length));

      Ada.Streams.Stream_IO.Close (Output_File);
   end Write;

end Gnatformat.Helpers;
