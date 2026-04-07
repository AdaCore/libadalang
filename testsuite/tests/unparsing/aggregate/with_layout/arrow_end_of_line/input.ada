package Command_Line is
   String_Infos : constant String_Option_Info_Array :=
     (Opt_Coverage_Level =>
        Create
          (Long_Name => "--level",
           Commands  =>
             (Cmd_Run | Cmd_Coverage | Cmd_Convert | Cmd_Instrument_With_Setup =>
                True,
              other                                                            =>
                False)));
end Command_Line;
