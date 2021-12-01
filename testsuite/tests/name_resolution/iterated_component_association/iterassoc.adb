package body Iterassoc is

    procedure P is
        type R is range 1 .. 5;
        type A is array (R) of Float;

        X : A := (1.0, 2.0, 3.0, 4.0, 5.0);

        N : A := (for I in 1 .. 2 => X(I) * X(I));
        pragma Test_Statement;

        M : A := (for I : Integer in reverse R => X(I) * X(I));
        pragma Test_Statement;

        O : A := (for I : Float of X => I * I);
        pragma Test_Statement;

    begin
        null;
    end P;

end Iterassoc;
