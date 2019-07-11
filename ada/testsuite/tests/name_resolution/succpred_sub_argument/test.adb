procedure Test is
   b_true : Boolean := True;
   result : Boolean;
begin
 result := Boolean'succ( Boolean'pred( b_true  ));
 pragma Test_Statement;
end Test;
