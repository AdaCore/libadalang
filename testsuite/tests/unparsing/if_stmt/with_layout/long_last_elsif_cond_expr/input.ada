if Subp.P_Is_Ghost_Code or else Is_Function_With_Ghost_Return (Subp) then
   return
     Non_Fuzzable_Subprogram'
       (Declaration => Subp.As_Basic_Decl,
        Reason      =>
          +Subp.Full_Sloc_Image & "Can't fuzz ghost code subprograms.");
elsif Subp.Kind in Ada_Abstract_Subp_Decl then
   return
     Non_Fuzzable_Subprogram'
       (Declaration => Subp.As_Basic_Decl,
        Reason      =>
          +Subp.Full_Sloc_Image & "Can't fuzz abstract subprograms.");
elsif Get_Subp_Params (Subp).Is_Null then
   return
     Non_Fuzzable_Subprogram'
       (Declaration => Subp.As_Basic_Decl,
        Reason      =>
          +Subp.Full_Sloc_Image
          & "Can't subprograms with no parameters.");
elsif not Has_Input_Parameters (Subp) then
   return
     Non_Fuzzable_Subprogram'
       (Declaration => Subp.As_Basic_Decl,
        Reason      =>
          +Subp.Full_Sloc_Image
          & "Can't fuzz subprograms with no 'in' or "
          & "'in out' parameters");
elsif Is_Subp_Decl_An_Import_With_Expression (Subp) and not Is_Intrinsic_Subprogram (Subp) then
   return
     Non_Fuzzable_Subprogram'
       (Declaration => Subp.As_Basic_Decl,
        Reason      =>
          +Subp.Full_Sloc_Image
          & "Can't fuzz subprograms where Imports aspect "
          & "values are derived from expressions.");
elsif Is_Subp_Decl_Null (Subp)
  and not Is_Expr_Function (Subp)
  and not Is_Subp_Decl_An_Import (Subp)
then
   return
     Non_Fuzzable_Subprogram'
       (Declaration => Subp.As_Basic_Decl,
        Reason      =>
          +Subp.Full_Sloc_Image
          & "Can't fuzz subprograms with null "
          & "specifications.");
else
  null;
end if;
