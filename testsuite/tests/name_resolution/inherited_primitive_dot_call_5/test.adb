procedure Test is
   package Base is
      type Stream_Type is tagged null record;
   end Base;

   generic
      type T is new Base.Stream_Type with private;
   package Gen is
      type Stream is new T with null record;
   end Gen;

   package My_Streams is
      type Stream is new Base.Stream_Type with null record;

      procedure Open (Self : Stream; X : Integer) is null;
   end My_Streams;

   package Pkg is new Gen (My_Streams.Stream);

   X : Pkg.Stream;
begin
   X.Open (1);
   pragma Test_Statement;
end Test;
