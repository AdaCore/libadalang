procedure Test is
   type I is limited interface;

   protected type T is new I with
      procedure Foo;
   end T;
   pragma Test_Statement;

   protected body T is
      procedure Foo is null;
   end T;

   protected O is new I with
      procedure Foo;
   end O;
   pragma Test_Statement;

   protected body O is
      procedure Foo is null;
   end O;
begin
   null;
end Test;
