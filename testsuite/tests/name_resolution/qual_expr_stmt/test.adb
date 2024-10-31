with Machine_Code; use Machine_Code;

procedure Test is
   procedure Code is
   begin
      Asm_Insn'(Asm ("nop"));
      pragma Test_Statement_UID;
   end Code;
begin
   Code;
end Test;
