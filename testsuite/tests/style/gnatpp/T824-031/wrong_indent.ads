
package wrong_indent is

   type Color is
        (Red,
         Blue,
         Green);

         -- This is where the comment should be, but it is indented to the right
    X : Integer;

    for Color use
        (Red    =>  0,
         Blue    => 1,
         Green => 2);

           -- This is where the comment should be, but it is indented to the right
    Y : Integer;


    Y1 : Y_Type :=
        (X => 0,
         Y => 1,
         Z => 2);

      -- This is where the comment should be, but it is indented to the right
    Z : Integer;
  
end wrong_indent;
