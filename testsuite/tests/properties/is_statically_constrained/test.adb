procedure Test (X, Y : Integer) is
   subtype T1 is String;
   --% node.p_is_statically_constrained

   subtype T2 is T1 (1 .. 4);
   --% node.p_is_statically_constrained

   subtype T3 is T2;
   --% node.p_is_statically_constrained

   type T4 is new String;
   --% node.p_is_statically_constrained

   type T5 is new String (1 .. X);
   --% node.p_is_statically_constrained

   type T6 is new T5;
   --% node.p_is_statically_constrained

   type T7 is new String (1 .. 4);
   --% node.p_is_statically_constrained

   type R1 is null record;
   --% node.p_is_statically_constrained

   type R2 (C : Boolean) is tagged null record;
   --% node.p_is_statically_constrained

   type R3 is new R2 with record
      A : Integer;
   end record;
   --% node.p_is_statically_constrained

   type Arr1 is array (Integer range <>) of Integer;
   --% node.p_is_statically_constrained

   type Arr2 is array (Integer range 1 .. 2) of Integer;
   --% node.p_is_statically_constrained

   type Arr3 is array (Integer range X .. Y) of Integer;
   --% node.p_is_statically_constrained

   O : Integer;
   --% node.p_is_statically_constrained

   X : constant String := "a" & Integer'Image (X);
   --% node.p_is_statically_constrained

   generic
      type Formal_Int is range <>;
      --% node.p_is_statically_constrained

      type Formal_Discrete is (<>);
      --% node.p_is_statically_constrained

      type Formal_Arr is array (Positive range <>) of Integer;
      --% node.p_is_statically_constrained
   package Gen is
      subtype Int_Ref is Formal_Int;
      subtype Arr_Ref is Formal_Arr;
   end Gen;

   type Cons_Arr is array (Positive range <>) of Integer;

   --  From an instantiation, a formal is statically constrained iff its
   --  actual is.
   package My_Gen is new Gen (Integer, Boolean, Cons_Arr);
   --% subtypes = node.p_designated_generic_decl.findall(lal.SubtypeDecl)
   --% [s.p_is_statically_constrained for s in subtypes]
begin
   null;
end;
