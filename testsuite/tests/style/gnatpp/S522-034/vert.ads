--  GNATpp alingnment issues with the following lines

package Vert is

   --type comment
   type Fubar is
     (One,
      -- member One
      Two
      -- member two
      );

     -- function comment
   function Example return Boolean;


end Vert;
