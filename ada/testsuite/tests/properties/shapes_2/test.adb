procedure Main is
   package My_Package is
      type Base_Rec (Disc : Integer) is tagged private;
   private
      type Base_Rec (Disc : Integer) is tagged record
         case Disc is
         when 1 =>
            A : Integer;
         when 2 =>
            Z : Integer;
         when others =>
            B : Integer;
         end case;
      end record;
   end My_Package;

   use My_Package;
   type Derived_Rec is new Base_Rec with record
      D : Integer;
   end record;

   Obj : Derived_Rec := (
   --% node.p_expression_type.p_shapes()
begin
   null;
end Main;
