if From < 1 then Empty_Model
elsif Bt.First_Element.Frame_Id = 0
--  gdb returns frames only from the first one in CLI mode
then Empty_Model else View.Model
