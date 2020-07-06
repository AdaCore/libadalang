with Ada.Text_IO; use Ada.Text_IO;

separate (P) procedure X (F : Integer) is
begin
   Put_Line ("YO");
end X;
--% node.parent.f_name.p_referenced_decl()
