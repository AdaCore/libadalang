procedure Bar (X : Sub) is
   procedure Baz (X : Sub) is separate;
begin
   null;
end Bar;
--% node.p_enclosing_compilation_unit.p_imported_units()
