procedure Test is
   generic
      type T is private;
   package Maps is
      type Map is tagged null record;

      procedure Insert (Self : Map; Elem : T) is null;
   end Maps;

   package My_Map is new Maps (Integer);

   package My_Map_Map is new Maps (My_Map.Map);
   type My_Map_Map_Ext is new My_Map_Map.Map with null record;

   A : My_Map.Map;
   B : My_Map_Map.Map;
   C : My_Map_Map_Ext;
begin
   My_Map_Map.Insert (B, A);
   pragma Test_Statement;
   B.Insert (A);
   pragma Test_Statement;
   Insert (C, A);
   pragma Test_Statement;
   C.Insert (A);
   pragma Test_Statement;
end Test;
