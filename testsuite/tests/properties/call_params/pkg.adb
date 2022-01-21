package body Pkg is
   function F1 (A : Integer := 0; B : Integer := 1) return Integer is
   begin
      return A + B;
   end F1;
   procedure P1 (A : Integer := 0; B : Integer := 1) is
   begin
      null;
   end P1;

   function F2 (A, B : Integer := 1) return Integer is
   begin
      return A + B;
   end F2;
   procedure P2 (A, B : Integer := 1) is
   begin
      null;
   end P2;

   function F3 return Integer is
   begin
      return 0;
   end F3;
   procedure P3 is
   begin
      null;
   end P3;

   procedure Test is
      X : Integer;
   begin
      X := F1;
      --% node.f_expr.p_call_params
      X := F1 (0);
      --% node.f_expr.p_call_params
      X := F1 (0, 1);
      --% node.f_expr.p_call_params
      X := F1 (B => 2);
      --% node.f_expr.p_call_params
      P1;
      --% node.f_call.p_call_params
      P1 (0);
      --% node.f_call.p_call_params
      P1 (0, X);
      --% node.f_call.p_call_params
      P1 (B => X);
      --% node.f_call.p_call_params

      X := F2;
      --% node.f_expr.p_call_params
      X := F2 (0);
      --% node.f_expr.p_call_params
      X := F2 (0, 1);
      --% node.f_expr.p_call_params
      X := F2 (B => 2);
      --% node.f_expr.p_call_params
      P2;
      --% node.f_call.p_call_params
      P2 (0);
      --% node.f_call.p_call_params
      P2 (0, X);
      --% node.f_call.p_call_params
      P2 (B => X);
      --% node.f_call.p_call_params

      X := F3;
      --% node.f_expr.p_call_params
      P3;
      --% node.f_call.p_call_params
   end Test;
end Pkg;
