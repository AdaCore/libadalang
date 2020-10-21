with Interfaces; use Interfaces;
with System.Machine_Code; use System.Machine_Code;

procedure Test is
   Value : Unsigned_32 := 12;
   Result : Unsigned_32;
begin
   Asm ("incl %0",
        Inputs  => Unsigned_32'Asm_Input ("a", Value),
        Outputs => Unsigned_32'Asm_Output ("=a", Result));
   pragma Test_Statement_UID;
end Test;
