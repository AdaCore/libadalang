procedure Test is
   type T1 is (E1, E2);

   type T2 (D : T1 := E2) is record
      case D is
         when E1 =>
            C1 : Integer;
            C2 : Boolean;
         when E2 =>
            null;
      end case;
   end record;

   type T3 is record
      C1 : Integer;
      C2 : Boolean := True;
   end record;

   type T4 (D : T1 := E1) is record
      case D is
         when E1 =>
            C1 : Integer;
            C2 : Boolean;
         when E2 =>
            null;
      end case;
   end record;

   type T5 (D1, D2 : T1 := E1; D3 : T1 := E2) is record
      case D2 is
         when E1 =>
            C1 : Integer;
            C2 : Boolean;
         when E2 =>
            null;
      end case;
   end record;

   O : constant T2 (D => E2) := (others => <>);
   pragma Test_Statement;
   P : constant T2 (D => E1) := (E1, others => <>);
   pragma Test_Statement;
   Q : constant T2 (D => E2) := (D => E2);
   pragma Test_Statement;
   F : constant T2 (D => E1) := (D => E1, C1 => 1, C2 => True);
   pragma Test_Statement;

   G : constant T3 := (C1 => 1, others => <>);
   pragma Test_Statement;
   H : constant T3 := (C1 => 1, C2 => True);
   pragma Test_Statement;

   OO : constant T4 (D => E1) := (E1, C1 => 1, C2 => True);
   pragma Test_Statement;
   PP : constant T4 (D => E2) := (E2, others => <>);
   pragma Test_Statement;
   QQ : constant T4 (D => E1) := (E1, 1, others => <>);
   pragma Test_Statement;

   Z : constant T5 (D1|D2 => E1, D3 => E2) := (D3 => E2, others => <>);
   pragma Test_Statement;

   DEF1 : T2 := (D => <>);
   pragma Test_Statement;
   DEF2 : T4 := (D => <>, C1 => <>, C2 => <>);
   pragma Test_Statement;
   DEF3 : T5 := (D1 => <>, D2 => <>, D3 => <>, C1 => <>, C2 => <>);
   pragma Test_Statement;
   DEF4 : T5 := (D1 | D2 | D3 => <>, C1 => <>, C2 => <>);
   pragma Test_Statement;

   A : constant T3 := (C1|C2 => <>);
   pragma Test_Statement;
   B : constant T3 := (others => <>);
   pragma Test_Statement;
begin
   null;
end Test;
