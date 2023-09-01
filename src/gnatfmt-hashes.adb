--
--  Copyright (C) 2023, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Unchecked_Conversion;

with System.Storage_Elements;

package body Gnatfmt.Hashes is

   ------------------
   -- Hash_Address --
   ------------------

   function Hash_Address (Addr : System.Address) return Hash_Type is
      use System, System.Storage_Elements;

      Result : constant Integer_Address :=
        To_Integer (Addr) / (2 ** Ignored_LSB);

   begin
      return Hash_Type'Mod (Result);
   end Hash_Address;

   -----------------
   -- Hash_Access --
   -----------------

   function Hash_Access (Acc : Object_Access) return Hash_Type is
      use System;

      function Convert is new Ada.Unchecked_Conversion
        (Object_Access, System.Address);

      function Hash is new Hash_Address (Word_Size / Storage_Unit);

   begin
      return Hash (Convert (Acc));
   end Hash_Access;

end Gnatfmt.Hashes;
