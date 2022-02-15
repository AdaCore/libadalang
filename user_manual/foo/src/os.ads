package OS is
   # if OS = "linux" then
      Name : constant String := "gnu-linux";
   # elsif OS = "windows" then
      Name : constant String := "microsoft-windows";
   # else
      Name : constant String := "unknown";
   # end if;
end OS;
