package Pkg is
    procedure P (A, B, C : Integer; D : Character);
    --% node.f_subp_spec.p_formal_params

    type Enum is (On, Off);

    type R (Dis : Integer) is
        record
            A    : Integer := Dis;
            B    : Integer;
            C, D : Integer;
        end record;
    --% node.f_discriminants.p_formal_params
    --% node.f_type_def.f_record_def.f_components.p_formal_params

    type S (Var : Enum) is
        record
            A : Integer;
            case Var is
                when On =>
                    B : Integer;
                --% node.parent.parent.parent.f_components.p_formal_params
                when Off =>
                    C : Character;
                    D : Integer;
                --% node.parent.parent.parent.f_components.p_formal_params
            end case;
        end record;
    --% node.f_discriminants.p_formal_params
    --% node.f_type_def.f_record_def.f_components.p_formal_params
end Pkg;
