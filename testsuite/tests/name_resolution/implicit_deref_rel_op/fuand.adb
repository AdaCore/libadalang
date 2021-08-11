package body FUAND is

   function Andthen (Ops : Operands) return Boolean is
      Ops_Var : Operands := Ops; -- # decl

      I : Integer_Ref := Integer_Ref'(Ref => new Integer'(12));

      A : Integer;
      B : Boolean;
   begin

      A := I ** 2;

      B := I > 2;

      if Ops_Var (Op_A) > 0 and then Ops_Var (Op_B) > 0 then -- # eval0
         return True; -- # true
      else
         return False;  -- # false
      end if;
      pragma Test_Statement;
   end;
end;
