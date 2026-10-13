procedure Main (X : in out Integer; Y : Integer)
  with Depends => (X => (X, Y)), Global => (Input => G1, Output => G2)
is
   A : constant Foo := Bar'(A, B);
   B : constant Foo := Bar'(A=>A, B=>B);
begin

  F1 := (1, 2, B => 3);

  F2 := (B1 => 1, B2 => 2, others => 0);

  F3 := [B => Val_1, Q => 15];

end Main;
