select
   accept Start
     (Data_To_Monitor : Data_To_Monitor_Access;
      Directories     : GNATCOLL.VFS.File_Array)
   do
      Data := Data_To_Monitor;

      Monitor := new LSP_Monitor;
      Monitor.The_Server := Data_To_Monitor.Server;

      Dirs := new File_Array (1 .. Directories'Length);
      Free_Index := 1;
      for Dir of Directories loop
         if Dir.Is_Directory then
            Dirs (Free_Index) := Dir;
            Free_Index := Free_Index + 1;
         end if;
      end loop;
   end Start;
or
   accept Stop do
      Stop_Requested := True;
   end Stop;
end select;
