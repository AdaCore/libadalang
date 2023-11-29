procedure Ada2022 is
   procedure P (Input : in Integer; Reg1 : out Integer; Reg2 : out Integer)
      with Post => ((Reg1 /= 0)
      and then (Reg2 /= 0)
      and then (declare
                   pragma Assume (Integer'Size = 100);
                   P : constant Integer := Reg1;
                   pragma Assume (P < 100);
                   pragma Assume (P < 100);
                   Q : constant Integer := Reg2;
                   pragma Assume (Q < 100);
                begin
                   (P + Q) mod 2 = 0));
   pragma Test_Block;

   procedure P (Input : in Integer; Reg1 : out Integer; Reg2 : out Integer) is
   begin
      null;
   end P;
begin
   null;
end Ada2022;
