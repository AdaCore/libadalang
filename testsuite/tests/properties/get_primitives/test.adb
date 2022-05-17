package P1 is

   type I5 is interface;
   procedure Proc_1 (X : I5) is abstract;
   procedure Proc_2 (X : I5) is abstract;
   procedure Proc_3 (X : I5) is abstract;
   function F_4 (X : I5) return Integer is abstract;
   function F_5 (X : I5) return Integer is abstract;

   type I7 is interface;
   procedure Proc_1 (X : I7) is abstract;
   procedure Proc_2 (X : I7) is abstract;
   procedure Proc_3 (X : I7) is null;
   procedure Proc_4 (X : I7) is abstract;
   procedure Proc_5 (X : I7) is abstract;
   function Fun_6 (X : I7) return Integer is abstract;
   function Fun_7 (X : I7) return Integer is abstract;

   type I_5_7 is interface and I5 and I7;
   --% node.p_get_primitives()

end P1;

