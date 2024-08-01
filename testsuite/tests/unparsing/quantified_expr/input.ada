if (for some Change of Value.contentChanges => Change > 80)
  or else (for all Change of Value.contentChanges
           => Change.text.Character_Length)
then
   --  Don't dump very long changes to avoid stack overflow
   Self.Output.Put ("<some big change>", Ok);
end if;
