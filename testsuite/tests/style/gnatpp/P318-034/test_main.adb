-- Testing pretty printing of enums with inline documentation
procedure test_main is

   -- output of gnat pp, in two versions ...

   -- what a nice type!
   type My_Animal_1 is (DOG, -- very long test documentation with much characters, which build words that hopefully make sense
   -- together
   CAT, -- test docu
   COW -- test docu
   );

   -- what a nice type!
   type My_Animal_2 is
     (DOG, -- very long test documentation with much characters, which build words that hopefully make sense together
   CAT, -- test docu
   COW -- test docu
   );

   --   --!pp off
   --
   --  -- what we would like to have is something like this ...
   --  type My_Animal_3 is (DOG, -- very long test documentation with much characters, which build words that hopefully make sense
   --                            -- together
   --                       CAT, -- test docu
   --                       COW -- test docu
   --                      );

   --  -- ... or maybe like this ...
   --  type My_Animal_4 is (
   --     DOG, -- very long test documentation with much characters, which build words that hopefully make sense together
   --     CAT, -- test docu
   --     COW -- test docu
   --  );

   --  -- ... or like this.
   --  type My_Animal_5 is
   --    (
   --     DOG, -- very long test documentation with much characters, which build words that hopefully make sense together
   --     CAT, -- test docu
   --     COW -- test docu
   --    );

   --  --!pp on

   function some_sum
     (a : Integer; -- very long test documentation with much characters, which build words that hopefully make sense together
                   -- and caused a line break --- this works
      b : Integer; -- test docu
      c : Integer; -- test docu
      d : Integer -- very long test documentation with much characters, which build words that hopefully make sense together
                  -- and caused a line break --- this is shifted to the front
      )
      return Integer -- some sum
   is
   begin
      return a + d + d + c + b + a + d;
   end some_sum;

begin
   null;
end test_main;
