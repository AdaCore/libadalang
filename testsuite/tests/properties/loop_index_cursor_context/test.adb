procedure Test is
   type Index is (Low, High);
   --% index_type = node
   type Other_Index is (Low, High);
   type Cursor is array (Index) of Natural;

   type Seq is null record
     with Iterable =>
       (First       => First,
        Next        => Next,
        Has_Element => Has_Element,
        Element     => Element);

   function First (S : Seq) return Cursor is ((others => 1));
   function Next (S : Seq; C : Cursor) return Cursor is (C);
   function Has_Element (S : Seq; C : Cursor) return Boolean is (True);
   function Element (S : Seq; C : Cursor) return Integer is (1);

   Q : Seq;
begin
   for E of Q loop
      declare
         --  Query the overloaded literal before resolving its enclosing call
         Value : Natural := E'Loop_Index (Low);
         --% arg = node.f_default_expr.f_suffix[0].f_r_expr
         --% arg.p_referenced_decl()
         --% arg.p_expression_type == index_type
      begin
         null;
      end;
   end loop;
end Test;
