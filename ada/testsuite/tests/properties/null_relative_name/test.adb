procedure Test is
begin
   null;
exception
   when A => null;
   when B : C => null;
end Test;
