procedure Test is
   type T is tagged null record;

   procedure Foo (T : T'Class) is null;
   pragma Test_Block;
begin
end Test;
