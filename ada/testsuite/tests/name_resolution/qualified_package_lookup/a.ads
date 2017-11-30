package A is
   type Integer is range 1 .. 10;
   Foo : Integer;
   package B is
      package C is
         pragma Test (Foo);
      end C;
   end B;
end A;
