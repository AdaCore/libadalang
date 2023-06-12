procedure Test is

   generic
      type T;
      pragma Test_Statement;
      --  A generic formal incomplete type decl: nothing to resolve
   package P1 is
   end P1;

   package P2 is
      type T;
      pragma Test_Statement;
      --  An incomplete type decl: nothing to resolve

      type T is new Integer;
   end P2;

   generic
      type T is tagged or use Positive;
      --  Interestingly, this formal incomplete type decl is not accepted by
      --  GNAT without the `is tagged` keywords, but the RM (12.5) allows it.
      pragma Test_Statement;
      --  A generic formal incomplete type decl with a default value (an Ada
      --  2022 feature): resolve the identifier referring to the default type
      --  to use.
   package P3 is
   end P3;

begin
   null;
end Test;
