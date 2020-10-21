package Foo is

   Empty        : constant String := "";
   Quote        : constant String := """";
   Percents     : constant String := %100%%!%;
   No_Bracket_1 : constant String := "[a";
   No_Bracket_2 : constant String := "[""";
   No_Bracket_3 : constant String := "[""]";
   Eacute       : constant Wide_Wide_String := "-é-";
   Brackets_1   : constant Wide_Wide_String := "["1"]"; --  Invalid
   Brackets_2   : constant Wide_Wide_String := "["e9"]";
   Brackets_4   : constant Wide_Wide_String := "["03C0"]";
   Brackets_5   : constant Wide_Wide_String := "["1F64F"]"; --  Invalid
   Brackets_6   : constant Wide_Wide_String := "["01F64F"]";
   Brackets_8   : constant Wide_Wide_String := "["000003A0"]";

end Foo;
