function Line_Terminator
  (Self : Text_Document'Class) return VSS.Strings.Virtual_String
is
   use type VSS.Strings.Virtual_String;

begin
   return
     (if Self.Line_Terminator.Is_Empty
      then
        --  Comments after if
        1 * VSS.Characters.Latin.Line_Feed
      --  Comments before elsif
      elsif Self.Line_Terminator.Something
      then
        --  Comments after elsif
        1 * VSS.Characters.Latin.Line_Feed
      --  Comments before else
      else
        --  Comments after else
        Self.Line_Terminator);

   return (if A then B  -- Comment after then-expr
           else C);
end Line_Terminator;
