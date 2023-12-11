procedure Derived is
   function F (S : String) return Boolean is (True);

   type String_T is new String with
      Dynamic_Predicate => F (String (String_T (1 .. 90)));
   pragma Test_Block;

   type Count is new Integer with
      Static_Predicate => Count /= 10;
   pragma Test_Block;

   type String_T2 is new String with
      Predicate => F (String (String_T2 (1 .. 90)));
   pragma Test_Block;

   type Count2 is new Integer with
      Predicate => Count2 /= 10;
   pragma Test_Block;

   type String_T3 is new String;
   pragma Predicate
     (Entity => String_T3,
      Check => F (String (String_T3 (1 .. 90))));
   pragma Test_Statement;

   type Count3 is new Integer;
   pragma Predicate
     (Entity => Count3,
      Check => Count3 /= 10);
   pragma Test_Statement;
begin
   null;
end Derived;
