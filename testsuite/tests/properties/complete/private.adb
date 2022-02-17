procedure P is
   package Q is
      procedure P (B : Boolean);
      private
         X : Boolean;
   end Q;

   package body Q is
      procedure P (B : Boolean) is
      begin
         null;
      end P;
   end Q;

begin
   Q.;
   --% list(node.f_call.p_complete)
end P;
