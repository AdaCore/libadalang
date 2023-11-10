with Ada.Characters.Latin_1;

procedure Test is
   type Character_Mapping is array (Character) of Character;

   package L renames Ada.Characters.Latin_1;

   Identity : constant Character_Mapping :=
     (L.NUL                         &  -- NUL                             0
      L.SOH                         &  -- SOH                             1
      L.STX                         &  -- STX                             2
      L.ETX                         &  -- ETX                             3
      L.EOT                         &  -- EOT                             4
      L.ENQ                         &  -- ENQ                             5
      L.ACK                         &  -- ACK                             6
      L.BEL                         &  -- BEL                             7
      L.BS                          &  -- BS                              8
      L.HT                          &  -- HT                              9
      L.LF                          &  -- LF                             10
      L.VT                          &  -- VT                             11
      L.FF                          &  -- FF                             12
      L.CR                          &  -- CR                             13
      L.SO                          &  -- SO                             14
      L.SI                          &  -- SI                             15
      L.DLE                         &  -- DLE                            16
      L.DC1                         &  -- DC1                            17
      L.DC2                         &  -- DC2                            18
      L.DC3                         &  -- DC3                            19
      L.DC4                         &  -- DC4                            20
      L.NAK                         &  -- NAK                            21
      L.SYN                         &  -- SYN                            22
      L.ETB                         &  -- ETB                            23
      L.CAN                         &  -- CAN                            24
      L.EM                          &  -- EM                             25
      L.SUB                         &  -- SUB                            26
      L.ESC                         &  -- ESC                            27
      L.FS                          &  -- FS                             28
      L.GS                          &  -- GS                             29
      L.RS                          &  -- RS                             30
      L.US                          &  -- US                             31
      L.Space                       &  -- ' '                            32
      L.Exclamation                 &  -- '!'                            33
      L.Quotation                   &  -- '"'                            34
      L.Number_Sign                 &  -- '#'                            35
      L.Dollar_Sign                 &  -- '$'                            36
      L.Percent_Sign                &  -- '%'                            37
      L.Ampersand                   &  -- '&'                            38
      L.Apostrophe                  &  -- '''                            39
      L.Left_Parenthesis            &  -- '('                            40
      'Z'                           &  -- 'Z'                            90
      L.Left_Square_Bracket         &  -- '['                            91
      L.Reverse_Solidus             &  -- '\'                            92
      L.Right_Square_Bracket        &  -- ']'                            93
      L.Circumflex                  &  -- '^'                            94
      L.Low_Line                    &  -- '_'                            95
      L.Grave                       &  -- '`'                            96
      L.LC_A                        &  -- 'a'                            97
      L.LC_B                        &  -- 'b'                            98
      L.LC_C                        &  -- 'c'                            99
      L.LC_D                        &  -- 'd'                           100
      L.LC_E                        &  -- 'e'                           101
      L.LC_F                        &  -- 'f'                           102
      L.LC_G                        &  -- 'g'                           103
      L.LC_H                        &  -- 'h'                           104
      L.LC_I                        &  -- 'i'                           105
      L.LC_J                        &  -- 'j'                           106
      L.LC_K                        &  -- 'k'                           107
      L.LC_L                        &  -- 'l'                           108
      L.LC_M                        &  -- 'm'                           109
      L.LC_N                        &  -- 'n'                           110
      L.LC_O                        &  -- 'o'                           111
      L.LC_P                        &  -- 'p'                           112
      L.LC_Q                        &  -- 'q'                           113
      L.LC_R                        &  -- 'r'                           114
      L.LC_S                        &  -- 's'                           115
      L.LC_T                        &  -- 't'                           116
      L.LC_U                        &  -- 'u'                           117
      L.LC_V                        &  -- 'v'                           118
      L.LC_W                        &  -- 'w'                           119
      L.LC_X                        &  -- 'x'                           120
      L.LC_Y                        &  -- 'y'                           121
      L.LC_Z                        &  -- 'z'                           122
      L.Left_Curly_Bracket          &  -- '{'                           123
      L.Vertical_Line               &  -- '|'                           124
      L.Right_Curly_Bracket         &  -- '}'                           125
      L.Tilde                       &  -- '~'                           126
      L.DEL                         &  -- DEL                           127
      L.Reserved_128                &  -- Reserved_128                  128
      L.Reserved_129                &  -- Reserved_129                  129
      L.BPH                         &  -- BPH                           130
      L.NBH                         &  -- NBH                           131
      L.Reserved_132                &  -- Reserved_132                  132
      L.NEL                         &  -- NEL                           133
      L.SSA                         &  -- SSA                           134
      L.ESA                         &  -- ESA                           135
      L.HTS                         &  -- HTS                           136
      L.HTJ                         &  -- HTJ                           137
      L.VTS                         &  -- VTS                           138
      L.PLD                         &  -- PLD                           139
      L.PLU                         &  -- PLU                           140
      L.RI                          &  -- RI                            141
      L.SS2                         &  -- SS2                           142
      L.SS3                         &  -- SS3                           143
      L.DCS                         &  -- DCS                           144
      L.PU1                         &  -- PU1                           145
      L.PU2                         &  -- PU2                           146
      L.STS                         &  -- STS                           147
      L.CCH                         &  -- CCH                           148
      L.MW                          &  -- MW                            149
      L.SPA                         &  -- SPA                           150
      L.EPA                         &  -- EPA                           151
      L.SOS                         &  -- SOS                           152
      L.Reserved_153                &  -- Reserved_153                  153
      L.SCI                         &  -- SCI                           154
      L.CSI                         &  -- CSI                           155
      L.ST                          &  -- ST                            156
      L.OSC                         &  -- OSC                           157
      L.PM                          &  -- PM                            158
      L.APC                         &  -- APC                           159
      L.LC_Icelandic_Thorn          &  -- LC_Icelandic_Thorn            254
      L.LC_Y_Diaeresis);               -- LC_Y_Diaeresis                255
   pragma Test_Statement;
begin
   null;
end Test;

