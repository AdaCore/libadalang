procedure Call_Array is
   procedure Foo (X : Integer; Y, Z : Boolean) is null;

   type Array_Type is array (Integer range 1 .. 3) of Integer;
   type P_Access is access procedure (E, F : Integer);
   type P_Arr is array (Integer range 1 .. 10) of P_Access;

   P_A : P_Arr;

begin
   Foo (Y => True, Z => False, X => 3);
   --% node.f_call.p_call_params
   Foo (42, Y => True, Z => False);
   --% node.f_call.p_call_params
   Foo (42, True, False);
   --% node.f_call.p_call_params
   P_A (4) (F => 42, E => 28);
   --% node.f_call.p_call_params
   P_A (5).all (1, 2);
   --% node.f_call.p_call_params
end Call_Array;
