procedure Test is
   -- ProtectedTypeDecl

   protected type T is
      entry E;
   private
      N : Natural;
   end T;

   protected body T is
      entry E when True is
      begin
         N := 1;
         pragma Test_Statement;
         T.N := 1;
         pragma Test_Statement;
      end E;
   end T;

   -- SingleProtectedDecl

   protected P is
      entry E;
   private
      N : Natural;
   end P;

   protected body P is
      entry E when True is
      begin
         N := 1;
         pragma Test_Statement;
         P.N := 1;
         pragma Test_Statement;
      end E;
   end P;

begin
   null;
end Test;
