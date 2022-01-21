procedure Dot is
   package P1 is
      procedure P (A : Integer := 0);
   end P1;
   package P2 is
      procedure P (A : Integer := 0);

      type T is tagged null record;
      procedure PT (S : T; V : Integer := 0) is null;
      procedure PW (S : T; V : Integer := 0; W : Integer := 1) is null;
   end P2;

   package body P1 is
      procedure P (A : Integer := 0) is
      begin
         null;
      end P;
   end P1;
   package body P2 is
      procedure P (A : Integer := 0) is
      begin
         null;
      end P;
   end P2;

   X : P2.T;
begin
   P1.P (A => 0);
   --% node.f_call.p_call_params
   P1.P (0);
   --% node.f_call.p_call_params
   P1.P;
   --% node.f_call.p_call_params
   P2.P (0);
   --% node.f_call.p_call_params
   P2.P;
   --% node.f_call.p_call_params
   X.PT (0);
   --% node.f_call.p_call_params
   X.PT (V => 0);
   --% node.f_call.p_call_params
   X.PT;
   --% node.f_call.p_call_params
   X.PW (0);
   --% node.f_call.p_call_params
   X.PW (V => 0);
   --% node.f_call.p_call_params
   X.PW;
   --% node.f_call.p_call_params
   X.PW (0, 0);
   --% node.f_call.p_call_params
   X.PW (V => 0, W => 0);
   --% node.f_call.p_call_params
   X.PW (0, W => 0);
   --% node.f_call.p_call_params
end Dot;
