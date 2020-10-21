package Pkg is

   procedure P;
   pragma Import (C, P);
   procedure P (I : Integer);

   procedure Q;
   procedure Q (I : Integer) with Import;

end Pkg;
