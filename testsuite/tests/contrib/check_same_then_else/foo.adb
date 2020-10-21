procedure Foo is

   B : Boolean := (

      --  Cases of normal tokens following an END

      Token = Tok_Case   or else
      Token = Tok_For    or else
      Token = Tok_If     or else
      Token = Tok_Loop   or else
      Token = Tok_Record or else
      Token = Tok_Select or else

      --  Cases of bogus keywords ending loops

      Token = Tok_For    or else
      Token = Tok_While);

   Result : constant Boolean :=
     (not Is_Singleton_Set (Set1))
      and then (not Is_Singleton_Set (Set1))
      and then (Num_Range_Pairs (Set1, Set2) > Range_Pair_Limit);

begin
   if S1.Label = null then
      S2.Label := null;
      S2.Label := null;
      S2.Label := null;
      S2.Label := null;
      S2.Label := null;
      S2.Label := null;
   elsif S2.Icon_Name = null then
      S2.Label := null;
      S2.Label := null;
      S2.Label := null;
      S2.Label := null;
      S2.Label := null;
      S2.Label := null;
   end if;
end Foo;
