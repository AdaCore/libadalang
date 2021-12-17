procedure Qual is
   type Code is (Fix, Cla, Dec, Tnz, Sub);
begin
   for J in Code'(Fix) .. Code'(Sub) loop
      null;
   end loop;
   --% [x.p_is_constant for x in node.findall(lal.QualExpr)]
end Qual;
