--
--  Copyright (C) 2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--
--  Package with the public API for editing sources

with GNATCOLL.VFS;

package Gnatformat.Range_Format is

   procedure Proceed_With_Range_Formatting
     (Project_File : GNATCOLL.VFS.Virtual_File);
   --  If range format mode is activated, process the selection and
   --  format it.
   --  If --pipe is used, then prints the formatted source selection to
   --  stdout.
   --  TBD: implement the part applying the formatted selection to the
   --       source file.

end Gnatformat.Range_Format;
