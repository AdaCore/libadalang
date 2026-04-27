procedure Test is
   package Pkg is
      --  Constant indexing returns an integer-range type
      type CI_Result is range 1 .. 100;

      --  Variable indexing returns a reference type whose dereferenced
      --  type is Boolean-derived, which is incompatible with CI_Result.
      type Mutable_T is new Boolean;
      type VI_Result (R : access Mutable_T) is null record
      with Implicit_Dereference => R;

      Target : aliased Mutable_T := True;

      --  Type with both Constant_Indexing and Variable_Indexing
      type Container is tagged null record
      with Constant_Indexing => Get_Const, Variable_Indexing => Get_Var;

      function Get_Const (C : Container; I : Integer) return CI_Result
      is (42);
      function Get_Var (C : Container; I : Integer) return VI_Result
      is ((R => Target'Access));

      --  Type with only Constant_Indexing: CI is always used
      type CI_Container is tagged null record
      with Constant_Indexing => Get_Const_Only;

      function Get_Const_Only (C : CI_Container; I : Integer) return CI_Result
      is (42);

      --  Type with only Variable_Indexing: VI is always used
      type VI_Container is tagged null record
      with Variable_Indexing => Get_Var_Only;

      function Get_Var_Only (C : VI_Container; I : Integer) return VI_Result
      is ((R => Target'Access));

      --  Procedures to probe parameter mode detection.
      --  Each procedure only accepts the result of one of the two
      --  indexing functions, making the expected resolution unambiguous.
      procedure Take_In (X : CI_Result) is null;
      procedure Take_In_Ref (X : Mutable_T) is null;
      procedure Take_Out (X : out VI_Result) is null;
      procedure Take_In_Out (X : in out VI_Result) is null;
      procedure Take_Mixed (X : CI_Result; Y : out VI_Result) is null;
      --  Three-parameter procedure with an out parameter in the middle, used
      --  to verify that in-position actuals resolve to CI even when another
      --  parameter of the same call is out.
      procedure Take_Mixed_3 (X : CI_Result; Y : out VI_Result; Z : CI_Result)
      is null;
   end Pkg;

   use Pkg;

   C   : Container := (null record);          --  variable object
   C_K : constant Container := (null record); --  constant object

   CI_C : CI_Container := (null record);
   VI_C : VI_Container := (null record);

   --  Object renaming: variable indexing
   R_Var : VI_Result renames C (1);
   pragma Test_Statement;  --  C (1) -> Get_Var

   --  Object renaming with VI-only type: no Constant_Indexing aspect, so
   --  variable indexing is always used regardless of context.
   R_VI_Var : VI_Result renames VI_C (1);
   pragma Test_Statement;  --  VI_C (1) -> Get_Var_Only

   --  Object renaming with CI-only type: no Variable_Indexing aspect, so
   --  constant indexing is always used regardless of context.
   R_CI_Var : CI_Result renames CI_C (1);
   pragma Test_Statement;  --  CI_C (1) -> Get_Const_Only

   V : CI_Result;

begin
   --  LHS of assignment: variable indexing
   C (1) := True;
   pragma Test_Statement;  --  C (1) -> Get_Var

   --  Variable object on RHS: not in writable context -> constant indexing
   V := C (1);
   pragma Test_Statement;  --  C (1) -> Get_Const

   --  Constant object: always constant indexing
   V := C_K (1);
   pragma Test_Statement;  --  C_K (1) -> Get_Const

   --  in parameter: not writable -> constant indexing
   Take_In (C (1));
   pragma Test_Statement;  --  C (1) -> Get_Const

   --  out parameter: writable -> variable indexing
   Take_Out (C (1));
   pragma Test_Statement;  --  C (1) -> Get_Var

   --  in out parameter: writable -> variable indexing
   Take_In_Out (C (1));
   pragma Test_Statement;  --  C (1) -> Get_Var

   --  Named in parameter -> constant indexing
   Take_In (X => C (1));
   pragma Test_Statement;  --  C (1) -> Get_Const

   --  Named out parameter -> variable indexing
   Take_Out (X => C (1));
   pragma Test_Statement;  --  C (1) -> Get_Var

   --  Mixed positional: first param (in) -> constant, second (out) -> variable
   Take_Mixed (C (1), C (1));
   pragma Test_Statement;  --  C (1) at X -> Get_Const, C (1) at Y -> Get_Var

   --  Mixed reversed named associations: Y (out) -> variable,
   --  X (in) -> constant; this specifically exercises match_param_list
   --  for out-of-order actuals.
   Take_Mixed (Y => C (1), X => C (1));
   pragma Test_Statement;  --  C (1) at X -> Get_Const, C (1) at Y -> Get_Var

   --  in-position actuals should resolve to CI even when another parameter of
   --  the same call is out; match_param_list correctly identifies the mode of
   --  each individual actual.
   Take_Mixed_3 (C (1), C (1), C (1));
   pragma Test_Statement;
   --  C (1) at X -> Get_Const, C (1) at Y -> Get_Var, C (1) at Z -> Get_Const

   --  named associations in reverse order
   Take_Mixed_3 (Z => C (1), Y => C (1), X => C (1));
   pragma Test_Statement;
   --  C (1) at X -> Get_Const, C (1) at Y -> Get_Var, C (1) at Z -> Get_Const

   --  Type with only Constant_Indexing: CI is always used regardless of context

   V := CI_C (1);
   pragma Test_Statement;  --  CI_C (1) -> Get_Const_Only

   Take_In (CI_C (1));
   pragma Test_Statement;  --  CI_C (1) -> Get_Const_Only

   --  Type with only Variable_Indexing: VI is always used regardless of context

   VI_C (1) := True;
   pragma Test_Statement;  --  VI_C (1) -> Get_Var_Only

   Take_Out (VI_C (1));
   pragma Test_Statement;  --  VI_C (1) -> Get_Var_Only

   Take_In_Out (VI_C (1));
   pragma Test_Statement;  --  VI_C (1) -> Get_Var_Only

   --  in parameter via implicit dereference of VI_Result
   Take_In_Ref (VI_C (1));
   pragma Test_Statement;  --  VI_C (1) -> Get_Var_Only

end Test;
