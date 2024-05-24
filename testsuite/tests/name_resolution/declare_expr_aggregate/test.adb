pragma Ada_2022;

procedure Test is
   type A is array (Natural range 0 .. 1) of Integer;

   function F return A is
     (declare
        I : constant Integer := 0;
        J : constant Integer := 1;
      begin (I, J));
   pragma Test_Statement;

   function G return Integer is
     (declare
        K : constant Integer := 2;
      begin (K));
   pragma Test_Statement;

   type B is array (Natural range 0 .. 1) of A;
   K : Integer := 3;

   function H return B is
     (declare
        K : constant Integer := 2;
      begin (B'First => (K, K), B'Last => (K, K)));
   pragma Test_Statement;
begin
   null;
end;
