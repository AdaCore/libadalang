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
begin
   null;
end;
