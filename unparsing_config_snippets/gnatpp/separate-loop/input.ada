procedure Separate_Loop is
begin

   while Foo loop
     null;
   end loop;

   for Foo of Bar when Foo.Is_Baz loop
     null;
   end loop;

end Separate_Loop;
