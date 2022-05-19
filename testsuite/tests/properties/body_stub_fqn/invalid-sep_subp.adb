separate (Invalid)
procedure Sep_Subp is
   package Nested is
      procedure P;
   end Nested;

   package body Nested is
      procedure P is separate;
      --% node.p_syntactic_fully_qualified_name
   end Nested;
begin
   declare
      procedure P is separate;
      --% node.p_syntactic_fully_qualified_name
   begin
      null;
   end;
end;
