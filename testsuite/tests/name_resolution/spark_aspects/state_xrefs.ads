package State_Xrefs
  with Abstract_State => State,
       Initializes    => State
is
   procedure Proc (Param_1 : Integer; Param_2 : out Integer)
     with Global  => (In_Out => State),
          Depends => ((Param_2, State) => (Param_1, State));

private
   package Nested_Private
     with Abstract_State => (Nested_State with Part_Of => State)
   is
      Var : Integer := 1234;
   end Nested_Private;
end State_Xrefs;
pragma Test_Block;
