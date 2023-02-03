procedure Gen_Incomplete is
   generic
      type T;
   package Pkg_G is
   end Pkg_G;

   package Foo is
      type T is private;

      package My_Pkg is new Pkg_G (T);
      --% node.p_inst_params

   private
      type T is record
         X : Integer;
      end record;
   end Foo;
begin
   null;
end Gen_Incomplete;
