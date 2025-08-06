--
--  Copyright (C) 2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Gnatformat.Helpers;

package body Gnatformat.File_Writers is

   ------------------
   -- Print_Source --
   ------------------

   overriding
   procedure Print_Source
     (Self : in out File_Writer;
      File : String;
      Text : Ada.Strings.Unbounded.Unbounded_String)
   is
      pragma Unreferenced (Self);
   begin
      Gnatformat.Helpers.Write (File, Text);
   end Print_Source;

end Gnatformat.File_Writers;
