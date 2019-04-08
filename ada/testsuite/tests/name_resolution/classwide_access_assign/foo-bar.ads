package Foo.Bar is

   type U_Record is new T_Record with null record;

   procedure Test (X : access U_Record);

end Foo.Bar;
