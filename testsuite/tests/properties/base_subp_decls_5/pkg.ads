package Pkg is
   type T is abstract tagged private;
   procedure P (Self : T);
private
   type T is abstract tagged null record;
   type T_2 is new T with null record;

   overriding
   procedure P (Self : T_2);
   --% node.p_base_subp_declarations()
end Pkg;

