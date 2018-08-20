separate (Pkg)
protected body P_Rec is

   entry E (I : in out Integer) when State > 0 is
   begin
      I := State;
   end E;

   procedure P (B : Boolean; I : out Integer) is
   begin
      if B then
         I := State;
      else
         I := 0;
      end if;
   end P;

   function F return Integer is
   begin
      return State;
   end F;

end P_Rec;
