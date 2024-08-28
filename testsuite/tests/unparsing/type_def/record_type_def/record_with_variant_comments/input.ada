type R (D : Discr_Type) is record
case D is
when A =>
Comp_A1 : A1_Type;
--  Comment for Comp_A1

Comp_A2 : A2_Type;
--  Comment for Comp_A2

when B =>
Comp_B : B_Type;
--  Comment for Comp_B
end case;
end record;
