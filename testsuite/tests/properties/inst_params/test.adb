procedure Test is
   package Pkg_Foo is
      generic
         with function F (X : Integer) return Integer is <>;
      procedure Foo;

      procedure Foo is null;
   end Pkg_Foo;

   package Pkg_Bar is
      generic
         type T is private;
         with function F (X : T) return T is <>;
      procedure Bar;

      procedure Bar is null;
   end Pkg_Bar;

   package Basic is
      function F (X : Integer) return Integer is (X);
      function F (X : Float) return Float is (X);

      procedure My_Foo is new Pkg_Foo.Foo;
      --% node.p_inst_params

      procedure My_Int_Bar is new Pkg_Bar.Bar (T => Integer);
      --% node.p_inst_params

      procedure My_Float_Bar is new Pkg_Bar.Bar (T => Float);
      --% node.p_inst_params
   end Basic;

   package Generic_Actual is
      generic
         type T is private;
      function Make_F (X : T) return T;

      function Make_F (X : T) return T is (X);

      function F is new Make_F (T => Float);

      procedure My_Float_Bar is new Pkg_Bar.Bar (T => Float);
      --% node.p_inst_params
   end Generic_Actual;

   package Explicit is
      function G (X : Integer) return Integer is (X);

      procedure My_Foo is new Pkg_Foo.Foo (F => G);
      --% node.p_inst_params
   end Explicit;

   package Builtin_Operators is
      generic
         with function "+" (L, R : Integer) return Integer is <>;
      package Foo is
      end Foo;

      package My_Foo is new Foo;
      --% node.p_inst_params
   end Builtin_Operators;
begin
   null;
end Test;
