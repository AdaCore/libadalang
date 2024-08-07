function Is_Subp_Decl_An_Import_With_Expression
  (Subp : Basic_Decl'Class) return Boolean
is (declare
      Import_Aspect : constant Aspect :=
        Subp.P_Get_Aspect (Langkit_Support.Text.To_Unbounded_Text ("Import"));
    begin
      Exists (Import_Aspect)
      and then (To_Lower (+Value (Import_Aspect).Text) /= "false"));
