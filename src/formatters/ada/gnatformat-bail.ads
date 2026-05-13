--
--  Copyright (C) 2026, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Driver-wide non-local exit via exception.
--
--  Callers signal "stop the program now with this status" by calling Bail.
--  The main subprogram catches Bail_Out at its top level and returns
--  normally, which lets Ada finalization run.
--
--  Using an exception as an exit mechanism is deliberate. The direct
--  alternative, GNAT.OS_Lib.OS_Exit bypasses adafinal entirely.
--  Several elaboration-time allocations would then remain live at process
--  exit and Valgrind would report them as leaks.

package Gnatformat.Bail is

   Bail_Out : exception;

   procedure Bail (Status : Integer)
   with No_Return;
   --  Set the program's exit status and raise Bail_Out.

end Gnatformat.Bail;
