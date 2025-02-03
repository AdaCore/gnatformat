pragma Ada_2012;

with Interfaces.C;
with System;

package test is

   use Interfaces.C;

   type Pointer is new System.Address;

   type SSL_CTX    is new Pointer;
   type SSL_Handle is new Pointer;

   R_57 : constant access function (SSL : SSL_Handle; Buf : Pointer; Num : int) return int
     with Import, Convention => C;

   function SSL_read (SSL : SSL_Handle; Buf : Pointer; Num : int) return int
   renames R_57.all;

   R_58 : constant access function (SSL : SSL_Handle; Buf : Pointer; Num : int) return int
     with Import, Convention => C;

   function SSL_peek (SSL : SSL_Handle; Buf : Pointer; Num : int) return int
   renames R_58.all;

   R_59 : constant access function (SSL : SSL_Handle; Buf : Pointer; Num : int) return int
     with Import, Convention => C;

   function SSL_write (SSL : SSL_Handle; Buf : Pointer; Num : int) return int
   renames R_59.all;

end test;