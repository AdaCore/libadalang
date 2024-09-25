procedure Test is
   I : Integer := 1;
   J : String := "Str";
begin
   F : Wide_Wide_String := f"fff";
   F := f"I: {I}, J: {J}";
   F := f"I";
   F := f"M}J}";
   F := f"\n";
end Test;
--% fstn = node.findall(lal.FormatStringTokNode)
--% [n.p_denoted_value for n in fstn]
