procedure Test is
   type Base is tagged null record;

   type Der;

   function Foo return access Der'Class is (null);

   X : access Der'Class := Foo;
   pragma Test_Statement;

   type Der is new Base with null record;
begin
   null;
end Test;

