package Prot is
   Var : Integer;

   protected P is
      entry E (I : in out Integer);
      procedure P (I : Integer);
   private
      Flag : Boolean;
   end P;

end Prot;
