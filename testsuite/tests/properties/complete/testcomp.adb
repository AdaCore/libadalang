with Pkg_1.Child;
with Test_Pkg;

procedure Testcomp is
   Inst : Test_Pkg.A;
begin
   Pkg_1.;
   --% list(node.f_call.p_complete)
   Pkg_1.Child.;
   --% list(node.f_call.p_complete)
   Pkg_1.Child2.;
   --% list(node.f_call.p_complete)
   --  Check that we can complete elements from a non-withed package (invisible
   --  items completion).


   --  Check that we don't see methods leaking from Test_Pkg's body. Also check
   --  that no methods are marked as not visible.
   Inst.;
   --% list(node.f_call.p_complete)
end Testcomp;
