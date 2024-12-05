function Analysis_Units return Libadalang.Analysis.Analysis_Unit_Array
is ((if Analyze_Mode_Switches.Get_Source /= No_File
     then
       [User_Project_Control_Access.all.Get_LAL_Context.Get_From_File
          (+Analyze_Mode_Switches.Get_Source.Full_Name)]
     else User_Project_Control_Access.all.Analysis_Units));
