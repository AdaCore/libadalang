package Derivations is

   type T1 is tagged null record;

   type T2 is tagged private;

   type T3 is new T1 with null record;

   type T4 is new T3 with null record;

private

   type T2 is new T1 with null record;

end Derivations;
