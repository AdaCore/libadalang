procedure SType is
    type T (K : Boolean) is null record;

    procedure Foo (X : Boolean) is
       -- Not static, because of non-static constraint
       subtype U1 is T (X);

       -- Static!

       subtype U2 is T (True);

       -- A subtype is not static if any Dynamic_Predicate specifications apply
       -- to it (RM 4-9 26/3).

       subtype U3 is T (True) with Dynamic_Predicate => (True);

    begin
       null;
    end Foo;
begin
   null;
end Stype;
