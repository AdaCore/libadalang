procedure Test is
   generic package GP is
      generic function GF return Boolean;
   private
      -- The following use clause was known to raise an infinite recursion in
      -- Name.name_designated_type_env because of an unsoundness issue between
      -- infinite recursion guards in lexical envs and memoized properties.

      use type Integer;
   end GP;

   package body GP is
      function GF return Boolean is (True);
   end GP;

   package P is new GP;
   function F is new P.GF;
   pragma Test_Statement;
begin
   null;
end Test;
