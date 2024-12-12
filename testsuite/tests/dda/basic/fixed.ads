package Fixed is
   type F1 is delta 0.1 range -1.0 .. 1.0;
   type F2 is delta 0.1 range -1.0 .. 1.0 with Small => 0.03333333;
   type F3 is delta 0.1 range -1.0 .. 1.0 with Small => 1.0 / 30.0;
   type F4 is delta 0.1 digits 14;
end Fixed;
