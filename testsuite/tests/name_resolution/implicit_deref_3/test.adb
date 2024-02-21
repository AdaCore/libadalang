procedure Test is

   package P is
      type T is tagged null record;
      type T_Element is access all T;

      function Key (This : in T) return Integer is (1);

      type T_Constant_Reference (Element : access constant T_Element) is private
         with Implicit_Dereference => Element;

      type MyArr (Capacity : Integer) is tagged private
         with Constant_Indexing => Query,
              Iterator_Element  => T_Element;

      type Elems_Access is access all MyArr;

      function Query(This: MyArr; Index : Integer) return T_Constant_Reference;
   private
      type T_Constant_Reference
        (Element : access constant T_Element) is null record;
      type MyArr (Capacity : Integer) is tagged null record;
   end P;

   package body P is
      function Query
        (This: MyArr; Index : Integer) return T_Constant_Reference is
          (T_Constant_Reference'(Element => null));

      procedure X (Elems : Elems_Access) is
         T : T_Element := Elems.all (1);
         pragma Test_Statement;
         C : T_Constant_Reference := Elems.all (1);
         pragma Test_Statement;
         I : Integer := Elems.all (1).all.Key;
         pragma Test_Statement;
      begin
         I := Elems.all (1).all.Key;
         pragma Test_Statement;
      end X;
   end P;

   use P;

   procedure Y (Elems : Elems_Access) is
      T : T_Element := Elems.all (1);
      pragma Test_Statement;
      C : T_Constant_Reference := Elems.all (1);
      pragma Test_Statement;
      I : Integer := Elems.all (1).all.Key;
      pragma Test_Statement;
   begin
      I := Elems.all (1).all.Key;
      pragma Test_Statement;
   end Y;

begin
   null;
end Test;
