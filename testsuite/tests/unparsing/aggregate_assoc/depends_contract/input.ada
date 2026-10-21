package Foo is
    procedure P
    with
      Depends => (A =>+ (E, I, J)),
      Blah    => (A => +(E, I, (Z => J, Y => K)), Depends => (A => +(T, U, V)));

    procedure Q
    with Depends => (X => null, null => X);
end Foo;
