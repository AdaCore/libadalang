package body Test is

   type Enum_1 is (A, B, C, D, E, F);
   --% node.p_discrete_static_values

   type Enum_2 is new Enum_1 range B .. D;
   --% node.p_discrete_static_values

   subtype Sub is Enum_1 range C .. E;
   --% node.p_discrete_static_values

   type Enum_3 is new Sub;
   --% node.p_discrete_static_values

   type Enum_4 is new Enum_1 with Static_Predicate => Enum_4 in A | C;
   --% node.p_discrete_static_values

   type Enum_5 is new Enum_4;
   --% node.p_discrete_static_values

   type Enum_6 is new Enum_1 with Static_Predicate => Enum_6 in A .. B | D .. F;
   --% node.p_discrete_static_values

   type Enum_7 is new Enum_1 with Static_Predicate => not (Enum_7 in B .. C);
   --% node.p_discrete_static_values

end Test;
