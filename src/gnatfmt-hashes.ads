--
--  Copyright (C) 2023, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Containers; use Ada.Containers;

with System;

--  Various helpers to work with hashes for standard containers

package Gnatfmt.Hashes is

   Initial_Hash : constant Hash_Type;

   generic
      Ignored_LSB : Natural;
      --  Amount of least significant bits to ignore for hashing
   function Hash_Address (Addr : System.Address) return Hash_Type;
   --  Compute the hash of an address, ignoring the given amount of least
   --  significant bits.

   generic
      type Object_Type (<>) is limited private;
      type Object_Access is access Object_Type;
   function Hash_Access (Acc : Object_Access) return Hash_Type;
   --  Generic access hash function

private

   Initial_Hash : constant Hash_Type := 0;

end Gnatfmt.Hashes;
