procedure Test is
   type Base is tagged null record;

   type T (X : Natural) is new Base with record
      case X is
         when 1 .. 10 =>
            C_A : Integer;
         when others =>
            null;
      end case;
   end record;

   type U is new T (8) with record
      C_B : Integer;
   end record;

   R : U := (X => 8, C_A => 13, C_B => 14);
   pragma Test_Statement;
begin
   null;
end Test;
