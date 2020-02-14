procedure A is
    type T is mod 12345;
    Z : T := 0;
begin
    Z := (3 * Z) - 1;
    pragma Test_Statement;
end A;
