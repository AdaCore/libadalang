case Foo is
   when Bar | Qux | Corge => null;

   when Baz | Qux => null;

   when others => null;
end case;
