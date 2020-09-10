package body Test is

   --  Test 1: no box

   procedure Foo is
   begin
      null;
   end Foo;

   --  Test 2: wrong box

   ---------
   -- Bar --
   ---------

   procedure Foo is
   begin
      null;
   end Foo;
end Test;
