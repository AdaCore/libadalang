package Test is
   procedure Foo is null
      with Inline => True;
   pragma Test_Block;
end Test;
