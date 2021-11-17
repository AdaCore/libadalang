package body Pkg is
    task body T is
    begin
        accept E;
        null;
    end T;

    procedure P is
        task T is
            entry E;
            --% node.p_accept_stmts
        end T;

        task body T is
        begin
            accept E;
            Pkg.T.E;
        end T;
    begin
        T.E;
    end P;

    task type TT is
        entry E;
        --% node.p_accept_stmts
    end TT;

    task body TT is
    begin
        accept E;
        null;
    end TT;

    procedure Q is
        A_Task : TT;
    begin
        A_Task.E;
    end Q;

end Pkg;
