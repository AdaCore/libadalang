procedure Test is
   package P is
      type T is private;

      function G (N : T) return Integer;
      function F (N : T) return Integer;
   private
      type T is tagged record
         Data : Integer;
      end record;
   end P;

   package body P is
      function G (N : T) return Integer is
      begin
        return N.F;
        pragma Test_Statement;
      end G;

      function F (N : T) return Integer is
      begin
        return N.Data;
      end F;
   end P;
begin
   null;
end Test;
