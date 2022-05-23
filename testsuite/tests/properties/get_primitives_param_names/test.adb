procedure Test_It is
   package P1 is
      type Parent is tagged null record;

      function Get1 (P : Parent) return Integer;
      function Get2 (P : Parent) return Integer;
   end P1;

   package body P1 is
      function Get1 (P : Parent) return Integer is (1);
      function Get2 (P : Parent) return Integer is (2);
   end P1;

   package P2 is
      --  Get2 is inherited from Parent, not Get1
      type Child is new P1.Parent with null record;
      --% node.p_get_primitives()

      overriding function Get1 (C : Child) return Integer;
   end P2;

   package body P2 is
      overriding function Get1 (C : Child) return Integer is (1);
   end P2;
begin
   null;
end Test_It;

