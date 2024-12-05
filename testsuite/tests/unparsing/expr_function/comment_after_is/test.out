function Get_Number_Of_Cores_To_Utilise_PRE return Boolean
is
   --  We have already dealt with the incorrect configuration when the user
   --  selects 1 core and hasn't disabled CMPLOG
   (not (Number_Of_Cores.Get = 1 and not Disable_CMPLOG.Get));
