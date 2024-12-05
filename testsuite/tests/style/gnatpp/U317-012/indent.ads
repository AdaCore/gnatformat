--  Initial formatting

package indent is

   --  the POK method
   function POK
     (V : Float)
      return Boolean;

   --  the P method
   function P
     (V1 : Float;
      V2 : Float)
      return Boolean;

   --  the Q procedure
   procedure Q
     (V : Float);

end indent;
