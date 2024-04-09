--
--  Copyright (C) 2024, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Containers.Vectors;
with Langkit_Support.Generic_API.Analysis;

package Gnatformat.Analysis_Unit_Vectors is new
   Ada.Containers.Vectors
     (Positive,
      Langkit_Support.Generic_API.Analysis.Lk_Unit,
      Langkit_Support.Generic_API.Analysis."=");
