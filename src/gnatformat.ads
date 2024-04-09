--
--  Copyright (C) 2024, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with GNATCOLL.Traces;

package Gnatformat is

   Gnatformat_Trace : GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create ("GNATFORMAT", GNATCOLL.Traces.Off);
   Version    : constant String := "debug";
   Build_Date : constant String := "debug";

end Gnatformat;
