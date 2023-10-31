procedure Test is
   generic
      type Elem_Type is abstract tagged private;
   package Lists is
      type List is tagged private;
   private
      type List is new Elem_Type with null record;
   end Lists;

   package Pkg is
   private
      type T is tagged null record;

      package T_Lists is new Lists (T);
      --% lists = node.p_designated_generic_decl
      --% base_expr = lists.find(lal.SubtypeIndication)
      --% base_expr.p_designated_type_decl
   end Pkg;
begin
   null;
end Test;
