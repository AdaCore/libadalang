with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   package Pkg is
      type T is tagged null record;

      function F0 return T;
      function F1 (X : T) return T;
      function F2 (X, Y : T) return T;
      procedure P2 (X, Y : T);
   end Pkg;

   package body Pkg is
      function F0 return T is
      begin
         Put_Line ("F0 of T");
         return (null record);
      end F0;
      function F1 (X : T) return T is
      begin
         Put_Line ("F1 of T");
         return X;
      end F1;
      function F2 (X, Y : T) return T is
      begin
         Put_Line ("F2 of T");
         return X;
      end F2;
      procedure P2 (X, Y : T) is
      begin
         Put_Line ("P2 of T");
      end P2;
   end Pkg;

   package Der is
      type U is new Pkg.T with null record;

      function F0 return U;
      function F1 (X : U) return U;
      function F2 (X, Y : U) return U;
      procedure P2 (X, Y : U);
   end Der;

   package body Der is
      function F0 return U is
      begin
         Put_Line ("F0 of U");
         return (null record);
      end F0;
      function F1 (X : U) return U is
      begin
         Put_Line ("F1 of U");
         return X;
      end F1;
      function F2 (X, Y : U) return U is
      begin
         Put_Line ("F2 of U");
         return X;
      end F2;
      procedure P2 (X, Y : U) is
      begin
         Put_Line ("P2 of U");
      end P2;
   end Der;

   use Pkg;

   X : T'Class := Der.U'(null record);
begin
   Put_Line ("Test 1");
   P2 (F0, F0);
   -- no dynamic controlling operand, so none of these are dispatching

   Put_Line ("Test 2");
   P2 (X, F0);
   -- X is dynamically tagged, so both F0 and P2 are dispatching

   Put_Line ("Test 3");
   P2 (F0, X);
   -- Same as above

   Put_Line ("Test 4");
   P2 (F1 (F1 (F0)), F1 (F1 (X)));
   -- Same as above but with more nesting

   Put_Line ("Test 5");
   P2 (F2 (F0, T'Class'(X)),
       F2 (F0, F1 (T'(F0))));
   -- Same but with more complex expressions
end Test;
