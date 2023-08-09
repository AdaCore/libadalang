procedure Test is
  generic
    type T is private;
    with function F (Obj : T) return Integer is <>;
  procedure PG;

  procedure PG is
    C : T;
  begin
     declare
        O : constant Integer := PG.F (C);
        pragma Test_Statement;
     begin
        null;
     end;
  end PG;
begin
   null;
end Test;
