procedure Test is
   package Pkg is
      type A (X : Integer) is null record;
      --% node.p_discriminants_list()

      type B is new A;
      --% node.p_discriminants_list()

      type C (X : Integer) is new A (X);
      --% node.p_discriminants_list()

      type D (Y : Integer) is private;
      --% node.p_discriminants_list()

      type E (<>) is private;
      --% node.p_discriminants_list()

      type F (V : Integer) is limited private;
      --% node.p_discriminants_list()

      type G (W : Integer) is limited private;
      --% node.p_discriminants_list()

      type H is null record;
      --% node.p_discriminants_list()

      subtype I is E;
      --% node.p_discriminants_list()
   private
      type D (Y : Integer) is null record;
      type E (X : Integer) is null record;

      task type F (V : Integer) is
      end F;

      protected type G (W : Integer) is
      end G;
   end Pkg;
   package body Pkg is
      subtype J is E;
      --% node.p_discriminants_list()

      task body F is
      begin
         null;
      end F;

      protected body G is
      end G;
   end Pkg;
begin
   null;
end Test;
