package Pkg is

   protected type Regular is
      procedure Set (I : Integer);
      function Get return Integer;
   private
      State : Integer;
   end;

   protected type P_Rec is
      entry E (I : in out Integer);
      procedure P (B : Boolean; I : out Integer);
      function F return Integer;
   private
      State : Integer;
   end;

end Pkg;
