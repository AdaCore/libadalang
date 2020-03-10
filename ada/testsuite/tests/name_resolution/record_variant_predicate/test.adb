procedure Test is
   type Enum is (A, B, C);

   subtype A_And_C is Enum with Predicate => A_And_C in A | C;
   subtype Only_A is Enum with Predicate => Only_A in A .. A;
   subtype Only_C is Only_A with Predicate => Only_C in A .. C;

   type R (K : Enum) is record
       case K is
          when A_And_C =>
             X : Integer;
          when others =>
             Y : Boolean;
       end case;
   end record;

   type S (K : Enum) is record
       case K is
          when A_And_C =>
             X : Integer;
          when others =>
             Y : Boolean;
       end case;
   end record;

   X : R := (A, 12);
   pragma Test_Statement;

   Y : S := (C, 12);
   pragma Test_Statement;
begin
   null;
end Test;
