package Gnatfmt.Optionals is

   generic
      type T is private;
   package Generic_Optional_Types is
      type Generic_Optional_Type (Is_Set : Boolean := False) is record
         case Is_Set is
            when False =>
               null;
            when True =>
               Value : T;
         end case;
      end record;
   end Generic_Optional_Types;

end Gnatfmt.Optionals;
