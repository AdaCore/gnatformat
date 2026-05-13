--
--  Copyright (C) 2026, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Command_Line;

package body Gnatformat.Bail is

   ----------
   -- Bail --
   ----------

   procedure Bail (Status : Integer) is
   begin
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Exit_Status (Status));
      raise Bail_Out;
   end Bail;

end Gnatformat.Bail;
