procedure Test is
   type T is range -10 .. 10;

   type F is digits 8;

   procedure Foo (X : T) is null;

   R : T;
   S : F;
begin
   Foo (2 + 2);
   pragma Test_Statement;

   S := F (3 * 2.4);
   pragma Test_Statement;
end Test;
