package P
  with Pure
is

   type T is new Integer with Atomic;

   procedure P with Inline;

   procedure Q (A : in Integer; B : out Positive)
     with Pre => A > 0, Post => B = A'Old;

end P;
