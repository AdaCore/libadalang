procedure Test is
   generic
      type Element_Type is private;
      type Array_Type is array (Positive) of Element_Type;
   procedure Generic_Constrained_Array_Sort (A : Array_Type);

   procedure Generic_Constrained_Array_Sort (A : Array_Type) is
      E : Element_Type := A (1);
   begin
      null;
   end Generic_Constrained_Array_Sort;

   generic
      type Element_Type is private;
      type Array_Type is array (Positive range <>) of Element_Type;
   procedure Generic_Array_Sort (Container : Array_Type);

   procedure Generic_Array_Sort (Container : Array_Type) is
      subtype Array_Subtype is Array_Type (Positive'First .. Positive'Last);
      procedure Sort is new Generic_Constrained_Array_Sort
        (Element_Type => Element_Type,
         Array_Type   => Array_Subtype);
   begin
      null;
   end Generic_Array_Sort;

   generic
      type Element_Type (<>) is private;
   package Indefinite_Vectors
   is
   private  -- failure only if private
      type Element_Access is access Element_Type;
      type Elements_Array is array (Positive range <>) of Element_Access;
      procedure Sort is new Generic_Array_Sort
        (Element_Type => Element_Access,
         Array_Type   => Elements_Array);
   end Indefinite_Vectors;

   package Err is new Indefinite_Vectors
     (Element_Type => Character);
   --% gen_pkg = node.p_designated_generic_decl
   --% sort_inst = gen_pkg.find(lal.GenericSubpInstantiation)
   --% gen_sort = sort_inst.p_designated_generic_decl
   --% gen_sort_body = gen_sort.p_body_part()
   --% constr_sort_inst = gen_sort_body.find(lal.GenericSubpInstantiation)
   --% gen_constr_sort = constr_sort_inst.p_designated_generic_decl
   --% sort_body = gen_constr_sort.p_body_part()
   --% e_decl = sort_body.find(lal.ObjectDecl)
   --% e_decl.f_default_expr.p_expression_type
begin
   null;
end Test;
