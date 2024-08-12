--
--  Copyright (C) 2024, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Common utility packages, subprograms and types

package Gnatformat.Utils is

   generic
      type T is private;
   package Optional is
      type Optional_Type (Is_Set : Boolean := False) is
         record
            case Is_Set is
               when True =>
                  Value : T;
               when False =>
                  null;
            end case;
         end record;

      None : constant Optional_Type := (Is_Set => False);

      function "Or" (Left : Optional_Type; Right : T) return T
      is (if Left.Is_Set then Left.Value else Right);
      --  Overwrite of the `or` operator.
      --
      --  If Left is set then its value is returned as the result.
      --  Otherwise Right is.

      function "Or"
        (Left, Right  : Optional_Type)
         return Optional_Type
      is (if Left.Is_Set then Left else Right);
      --  Overwrite of the `or` operator.
      --
      --  If Left is set then it's returned as the result. Otherwise Right is.

   end Optional;

end Gnatformat.Utils;
