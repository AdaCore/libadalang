package Pkg is
   type Enum_1 is (A, B, C);
   --% [l.p_enum_rep for l in node.findall(lal.EnumLiteralDecl)]

   type Enum_2 is (A, B, C);
   --% [l.p_enum_rep for l in node.findall(lal.EnumLiteralDecl)]
   for Enum_2 use (10, 20, 30);

   type Enum_3 is (A, B, C);
   --% [l.p_enum_rep for l in node.findall(lal.EnumLiteralDecl)]
   for Enum_3 use (A => -30, B => -20, C => -10);

   type Enum_4 is (A, B, C);
   --% [l.p_enum_rep for l in node.findall(lal.EnumLiteralDecl)]
   for Enum_4 use (10 ** 40,
                   10 ** 40 + 1,
                   10 ** 40 + 2);
end Pkg;
