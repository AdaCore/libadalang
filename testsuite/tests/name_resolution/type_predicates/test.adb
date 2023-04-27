procedure Test is
   function F (S : String) return Boolean is (True);

   subtype String_T is String (1 .. 99) with
      Dynamic_Predicate => F (String_T (1 .. 90));
   pragma Test_Block;

   subtype Count is Integer with
      Static_Predicate => Count /= 10;
   pragma Test_Block;

   subtype String_T2 is String (1 .. 99) with
      Predicate => F (String_T2 (1 .. 90));
   pragma Test_Block;

   subtype Count2 is Integer with
      Predicate => Count2 /= 10;
   pragma Test_Block;

   subtype String_T3 is String (1 .. 99);
   pragma Predicate
     (Entity => String_T3,
      Check => F (String_T3 (1 .. 90)));
   pragma Test_Statement;

   subtype Count3 is Integer;
   pragma Predicate
     (Entity => Count3,
      Check => Count3 /= 10);
   pragma Test_Statement;
begin
   null;
end Test;
