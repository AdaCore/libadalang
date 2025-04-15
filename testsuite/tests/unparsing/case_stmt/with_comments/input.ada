procedure Foo is
begin
   case Bar is
      -- Comment A

      when A =>
         -- Comment B
         null;

      -- Comment C

      when B =>
         -- Comment D
         null;

      -- Comment E

      when C =>
         -- Comment F
         null;

      -- Comment F
   end case;
end Foo;
