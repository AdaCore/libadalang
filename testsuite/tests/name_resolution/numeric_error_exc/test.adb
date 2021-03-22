procedure Test is
begin
   null;
exception
   when Numeric_Error =>
      null;
end Test;
pragma Test_Block;
