with Wat;

procedure Usealltype is
   use all type Wat.A;

   Inst : Wat.A;
begin
   Prim (Inst);
   pragma Test_Statement;
end Usealltype;
