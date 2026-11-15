      if From < 1 then
         Clear (View.Model);
      elsif Bt.First_Element.Frame_Id = 0
      --  gdb returns frames only from the first one in CLI mode
      then
         Clear (View.Model);
      end if;
