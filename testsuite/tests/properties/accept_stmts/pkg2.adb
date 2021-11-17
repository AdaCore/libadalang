package body Pkg2 is

    task T is
        entry E1;
        --% node.p_accept_stmts
        entry E2;
        --% node.p_accept_stmts
        entry E3;
        --% node.p_accept_stmts
        entry E4;
        --% node.p_accept_stmts
    end T;

    task body T is
    begin
        loop
            select
                accept E1 do
                    null;
                end E1;
            or
                accept E2 do
                    null;
                end E2;
            or
                accept E3 do
                    accept E1;
                    null;
                end E3;
            or
                terminate;
            end select;
            accept E1;
            null;
            accept E1;
            null;
            accept E1;
            null;
            accept E2;
            null;
        end loop;
    end T;

    procedure P is
    begin
        T.E1;
        T.E2;
        T.E3;
    end P;

end Pkg2;
