procedure Test is
   generic
      type T is (<>);
      with function FI (I: T) return String is T'Image;
      pragma Test_Statement;
      with function FV (S: String) return T is T'Value;
      pragma Test_Statement;
   package P is
      procedure P is null;
   end P;
begin
   null;
end Test;
