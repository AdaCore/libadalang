package body State_Xrefs
  with Refined_State => (State => (C1, C2, Nested_Private.Nested_State))
is
   C1 : Integer := 1;
   C2 : Integer := 2;

   procedure Proc (Param_1 : Integer; Param_2 : out Integer)
     with Refined_Global  =>  (In_Out           => (C1, C2)),
          Refined_Depends => ((Param_2, C1, C2) => (Param_1, C2),
                               null             =>  C1)
   is begin null; end Proc;

   package body Nested_Private
     with Refined_State => (Nested_State => C3)
   is
      C3 : Integer := 3;
   end Nested_Private;
end State_Xrefs;
pragma Test_Block;
