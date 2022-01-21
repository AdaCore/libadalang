package Pkg is
   function F1 (A : Integer := 0; B : Integer := 1) return Integer;
   procedure P1 (A : Integer := 0; B : Integer := 1);

   function F2 (A, B : Integer := 1) return Integer;
   procedure P2 (A, B : Integer := 1);

   function F3 return Integer;
   procedure P3;
end Pkg;
