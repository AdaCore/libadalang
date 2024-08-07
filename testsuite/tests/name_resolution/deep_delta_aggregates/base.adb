procedure Base with SPARK_Mode is

   --  Handling of simple aggregates with only record components

   procedure Test_Record with Global => null is
      type R1 is record
         F1, F2, F3 : Integer;
      end record;
      type R2 is record
         G1, G2, G3 : R1;
      end record;

      X : R2 := (others => (others => 1));
      Y : R2 := (X with delta G1.F1 => 2, G2.F2 => 3, G3 => (others => 4));
      pragma Test_Statement;
   begin
      null;
   end Test_Record;

   --  General case

   procedure Test_Array (I, J, C : Integer) with
     Global => null,
     Pre => I in 1 .. C and J in 1 .. C
   is
      type R1 is record
         F1, F2, F3 : Integer;
      end record;
      type R2 is record
         G1 : R1;
         G2, G3 : Integer;
      end record;
      type R2_Arr is array (Integer range 1 .. C) of R2;
      type R3 is record
         H1 : R2_Arr;
         H2 : Integer;
      end record;

      X : R3 := (H1 => (others => (G1 => (others => 1), others => 1)), H2 => 1);
      Y : R3 := (X with delta
                   H1 (I).G1 => (others => 2),
                   H1 (J).G1.F2 => 3,
                   H2 => 4);
      pragma Test_Statement;
   begin
      null;
   end Test_Array;

   --  Test with a nested array access

   procedure Test_Nested_Array (C : Integer) with
     Global => null
   is
      type R1 is record
         F1, F2, F3 : Integer;
      end record;
      type R1_Arr is array (Integer range 1 .. 10) of R1;
      type R2 is record
         G1 : R1_Arr;
         G2, G3 : Integer;
      end record;
      type R2_Arr is array (Integer range 1 .. C) of R2;
      type R3 is record
         H1 : R2_Arr;
         H2 : Integer;
      end record;

      --  Test with possibly overlapping components

      procedure Test (I, J, K, L, M, N, P, Q : Integer) with
        Global => C,
        Pre => I in 1 .. C and J in 1 .. C and L in 1 .. C and N in 1 .. C
        and K in 1 .. 10 and M in 1 .. 10 and P in 1 .. 10 and Q in 1 .. C
      is
         X : R3 :=
           (H1 => (others => (G1 => (others => (others => 1)), others => 1)),
            H2 => 1);
         Y : R3 := (X with delta
                      H1 (I).G1 => (others => (others => 2)),
                      H1 (J).G1 (K) => (others => 3),
                      H1 (L).G1 (M).F1 => 4,
                      H1 (N).G1 (P).F2 => 5,
                      H1 (Q).G2 => 6,
                      H2 => 7);
         pragma Test_Statement;
      begin
         null;
      end Test;
   begin
      null;
   end Test_Nested_Array;

begin
   null;
end Base;
