procedure Test is
   type T is null record with Value_Size => 10;
   --% node.p_get_aspect("Value_Size")
   --% node.p_has_aspect("Value_Size")

   type New_T_1 is new T with Value_Size => 8;
   --% node.p_get_aspect("Value_Size")
   --% node.p_has_aspect("Value_Size")

   type New_T_2 is new T;
   --% node.p_get_aspect("Value_Size")
   --% node.p_has_aspect("Value_Size")

   type New_T_3 is new T;
   --% node.p_get_aspect("Value_Size")
   --% node.p_has_aspect("Value_Size")
   for New_T_3'Value_Size use 7;

   subtype Sub_T_1 is T with Value_Size => 6;
   --% node.p_get_aspect("Value_Size")
   --% node.p_has_aspect("Value_Size")

   subtype Sub_T_2 is T;
   --% node.p_get_aspect("Value_Size")
   --% node.p_has_aspect("Value_Size")

   subtype Sub_T_3 is T;
   --% node.p_get_aspect("Value_Size")
   --% node.p_has_aspect("Value_Size")
   for Sub_T_3'Value_Size use 5;

   type A is array (1 .. 10) of Integer with Pack;
   --% node.p_get_aspect("Pack")
   --% node.p_has_aspect("Pack")

   type New_A is new A;
   --% node.p_get_aspect("Pack")
   --% node.p_has_aspect("Pack")

   subtype Sub_A is A;
   --% node.p_get_aspect("Pack")
   --% node.p_has_aspect("Pack")

   type New_S is new String;
   --% node.p_get_aspect("Pack")
   --% node.p_has_aspect("Pack")
   --% node.p_get_pragma("Pack")
   --% node.p_root_type().p_get_aspect("Pack")

   type TG is tagged null record with Value_Size => 128;
   --% node.p_get_aspect("Value_Size")

   type New_TG_1 is new TG with record X : Boolean; end record with Value_Size => 192;
   --% node.p_get_aspect("Value_Size")
   type New_TG_2 is new TG with record X : Boolean; end record;
   --% node.p_get_aspect("Value_Size")
   for New_TG_2'Value_Size use 192;

   type New_TG_3 is new TG with record X : Boolean; end record;
   --% node.p_get_aspect("Value_Size")

   subtype Sub_TG_1 is TG with Value_Size => 256;
   --% node.p_get_aspect("Value_Size")
   subtype Sub_TG_2 is TG;
   --% node.p_get_aspect("Value_Size")
   for Sub_TG_2'Value_Size use 192;
   subtype Sub_TG_3 is TG;
   --% node.p_get_aspect("Value_Size")

   type I is new T with Value_Size => 13;
   subtype J is I;
   type K is new J with Value_Size => 14;

   subtype Sub_T_4 is J;
   --% node.p_get_aspect("Value_Size")
   subtype Sub_T_5 is K;
   --% node.p_get_aspect("Value_Size")

   type New_New_T_1 is new New_T_1;
   --% node.p_get_aspect("Value_Size")

   type New_New_T_2 is new New_T_2;
   --% node.p_get_aspect("Value_Size")

   type New_New_T_3 is new New_T_3;
   --% node.p_get_aspect("Value_Size")

   type New_New_New_T_3 is new New_New_T_3 with Value_Size => 1024;
   --% node.p_get_aspect("Value_Size")
begin
   null;
end Test;
