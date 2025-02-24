procedure SType is
    type T (K : Boolean) is null record;

    procedure Foo (X : Boolean) is
       -- Not static, because of non-static constraint
       subtype U1 is T (X);

       -- Not static because T is not scalar!
       subtype U2 is T (True);
    begin
       null;
    end Foo;

    procedure Bar (A, B : Integer) is
       -- Not static because non static constraint
       subtype V1 is Natural range A .. B;

       -- Static!
       subtype V2 is Natural range 3 .. 5;

       -- A subtype is not static if any Dynamic_Predicate specifications apply
       -- to it (RM 4-9 26/3).
       subtype V3 is Natural range 3 .. 5 with Dynamic_Predicate => (True);
    begin
        null;
    end Bar;
begin
   null;
end Stype;
