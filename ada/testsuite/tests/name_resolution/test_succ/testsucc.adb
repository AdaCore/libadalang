procedure Testsucc is
   type Enum is (A, B, C, D);

   Inst : Enum := A;
   Inst_2 : Enum := Enum'Succ(Inst);
   Inst_3 : Enum := Enum'Succ(C);
begin
   null;
end Testsucc;
pragma Test_Block;
