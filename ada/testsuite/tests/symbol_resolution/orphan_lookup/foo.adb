package Foo is

   package Bar is
      type T1 is new Integer;
   end;

   type T2 is new Integer;

   I : Bar.T2;
end Foo;
