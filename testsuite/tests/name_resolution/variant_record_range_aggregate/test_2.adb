procedure Test_2 is
   type Enum is (A, B, C, D, E);
   subtype AC is Enum
      with Static_Predicate => (AC in A .. B or AC in C | D | E) and AC < D;
   subtype BC is Enum
      with Static_Predicate => BC >= B and BC /= D and BC /= E;
   subtype DE is Enum
      with Static_Predicate => DE = D or DE = E;

   type T_1 (K : Enum) is record
      case K is
         when AC =>
            C_AC : Integer;
         when others =>
            C_DE : Integer;
      end case;
   end record;

   type T_2 (K : Enum) is record
      case K is
         when A .. C =>
            C_AC : Integer;
         when D .. E =>
            C_DE : Integer;
      end case;
   end record;

   type T_3 (K : Enum) is record
      case K is
         when B | A | C =>
            C_AC : Integer;
         when D | E =>
            C_DE : Integer;
      end case;
   end record;

   procedure Foo (V_AC : AC; V_BC : BC; V_DE : DE) is
      T_1_AC : T_1 := (K => V_AC, C_AC => 1);
      pragma Test_Statement;

      T_1_BC : T_1 := (K => V_BC, C_AC => 1);
      pragma Test_Statement;

      T_1_DE : T_1 := (K => V_DE, C_DE => 1);
      pragma Test_Statement;

      T_2_AC : T_2 := (K => V_AC, C_AC => 1);
      pragma Test_Statement;

      T_2_BC : T_2 := (K => V_BC, C_AC => 1);
      pragma Test_Statement;

      T_2_DE : T_2 := (K => V_DE, C_DE => 1);
      pragma Test_Statement;

      T_3_AC : T_3 := (K => V_AC, C_AC => 1);
      pragma Test_Statement;

      T_3_BC : T_3 := (K => V_BC, C_AC => 1);
      pragma Test_Statement;

      T_3_DE : T_3 := (K => V_DE, C_DE => 1);
      pragma Test_Statement;
   begin
      null;
   end Foo;
begin
   null;
end Test_2;
