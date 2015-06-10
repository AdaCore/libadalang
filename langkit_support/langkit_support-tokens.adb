package body Langkit_Support.Tokens is

   -------------
   -- Compare --
   -------------

   function Compare (Left, Right : Source_Location) return Relative_Position is
   begin
      if Right.Line < Left.Line then
         return Before;
      elsif Left.Line < Right.Line then
         return After;

      elsif Right.Column < Left.Column then
         return Before;
      elsif Left.Column < Right.Column then
         return After;
      else
         return Inside;
      end if;
   end Compare;

   -------------
   -- Compare --
   -------------

   function Compare (Sloc_Range : Source_Location_Range;
                     Sloc       : Source_Location) return Relative_Position is
   begin
      return (case Compare (Start_Sloc (Sloc_Range), Sloc) is
                 when Before => Before,
                 when Inside => Inside,
                 when After  =>
                   (if Compare (End_Sloc (Sloc_Range), Sloc) = After
                    then After
                    else Inside));
   end Compare;

end Langkit_Support.Tokens;
