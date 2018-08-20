package Pkg is

   protected type P_Rec is
      entry E (I : in out Integer);
      procedure P (B : Boolean; I : out Integer);
      function F return Integer;
   private
      State : Integer;
   end;

end Pkg;
