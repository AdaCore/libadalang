procedure Test is
   package Seqs is
      type Seq is null record;

      function No_Seq return Seq is (null record);
   end Seqs;

   generic
      type Seq_T is new Seqs.Seq;
   package Pkg is
      procedure Test;
   end Pkg;

   package body Pkg is
      procedure Test is
         X : Seq_T := No_Seq;
      begin
         null;
      end Test;
   end Pkg;

   package My_Pkg is new Pkg (Seqs.Seq);
   pragma Test_Statement;
begin
   null;
end Test;
