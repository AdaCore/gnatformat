procedure main is

Me : constant Trace_Handle := Create ("Hello", Unit_Name => FooBar_1, Default => FooBar_2, Stream => FooBar_3, Factory => FooBar_4, Finalize => FooBar_5);

Me2 : constant Trace_Handle := Create ("Hello", (Unit_Name => FooBar_1, Default => FooBar_2, Stream => FooBar_3, Factory => FooBar_4), Finalize => FooBar_5);

begin

     null;

end main;
