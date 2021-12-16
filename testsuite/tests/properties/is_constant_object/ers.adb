function Ers return Integer is
   One : Integer := 1;
begin
   return Result : constant Integer := One;
   --% node.find(lal.ExtendedReturnStmtObjectDecl).p_is_constant_object
end Ers;
