with Ada.Containers.Ordered_Maps;

procedure Test is
   type Rec is record
      X : Integer;
   end record;

   package Lang_Value_List_Map is new
     Ada.Containers.Ordered_Maps
       (Positive, Rec);

   use Lang_Value_List_Map;

   package Lang_Value_List_Map_Maps is new
     Ada.Containers.Ordered_Maps
       (Positive,
        Lang_Value_List_Map.Map);

   M : Lang_Value_List_Map_Maps.Map;
begin
   M (1) (2).X := 3;
   pragma Test_Statement_UID;
end Test;

