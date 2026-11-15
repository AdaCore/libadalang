      if From < 1 or else Bt.First_Element.Frame_Id = 0
      --  gdb returns frames only from the first one in CLI mode
      then
         Clear (View.Model);
      end if;
