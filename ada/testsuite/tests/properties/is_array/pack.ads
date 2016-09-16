package Pack is

   type Integer is range 1 .. 100;
   type Natural is new Integer;
   type Boolean is (True, False);

   type T1;
   type T2 is private;
   type T3 is new Integer;
   type T4 is array (Natural range <>) of Integer;
   type T5 is access T4;

   subtype S1 is T1;
   subtype S2 is T2;
   subtype S3 is T3 range 1 .. 9;
   subtype S4 is T4;
   subtype S5 is T4 (1 .. 9);

   O_T1 : T1;
   O_T2 : T2;
   O_T3 : T3;
   O_T4 : T4;
   O_T5 : T5;

   O_A1 : array (Boolean) of Integer;
   O_A2 : T4 (1 .. 9);

   O_S1 : S1;
   O_S2 : S2;
   O_S3 : S3;
   O_S4 : S4;
   O_S5 : S5;
end Pack;
