 procedure Test (A : Foo; B : Bar) is
    C : Integer;
 begin
    for El : Foo_Elem of A loop
       B.RealBar (El);
    end loop;
 end Test;
