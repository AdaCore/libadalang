if Gnat2Why_Args.Proof_Warnings
  --  and warnings are not suppressed
  and then Opt.Warning_Mode
           /= Opt.Suppress
  --  and this specific warning is not suppressed
  and then Do_Warn
  --  and a warning can be issued on that node
  and then May_Issue_Warning_On_Node (N)
then
   null;
end if;
