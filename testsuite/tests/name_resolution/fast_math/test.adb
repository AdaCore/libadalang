-- Test for the Fast_Math pragma/attribute name resolution

pragma Fast_Math;
pragma Test_Statement;

procedure Test is
   FM_Enabled : Boolean := Standard'Fast_Math;
   pragma Test_Statement;
begin
   null;
end Test;
