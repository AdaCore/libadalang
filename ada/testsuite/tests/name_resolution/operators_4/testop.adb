procedure A is
    type T is range 1 .. 10;
    type U is range 1 .. 10;

    function "-" (Left : T; Right : U) return U is (1);

    N : constant := 5;

    X : constant T := 2;
    Y : T;
    Z : U;
begin
    Y := N * (X - 1);
    pragma Test_Statement;
end A;
