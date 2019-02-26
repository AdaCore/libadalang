package Pkg is
   type Base is tagged null record;

   type Derived is new Base with private;

private

   type Derived is new Base with record
      X : Integer;
   end record;

end Pkg;
