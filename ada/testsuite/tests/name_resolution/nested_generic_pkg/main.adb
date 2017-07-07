with A;

procedure Main is
   package A_Inst is new A (Integer);
   package B_Inst is new A_Inst.B (Float);

   F : Float;
begin
   F := B_Inst.Convert (12);
   pragma Test_Statement;
end Main;
