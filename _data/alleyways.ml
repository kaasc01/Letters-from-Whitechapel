(**************************************************************************)
(*                                                                        *)
(*  Letters from Whitechapel: A Model and Solver                          *)
(*  Copyright (C) 2018                                                    *)
(*  Christian Kaas                                                        *)
(*                                                                        *)
(*  Based on the Cobblepot Games' popular "Letters from Whitechapel"      *)
(*  boardgame by Gabriele Mari and Gianluca Santopietro                   *)
(*  http://www.sirchestercobblepot.com/en/letters-from-whitechapel/       *)
(*                                                                        *)
(**************************************************************************) 

(* The data below describes the linkages between circles on the Whitechapel
   board that are feasible through an alleyway move *)

let alleyways =
  [(1, [7; 26]);
   (2, [9]);
   (3, [4; 11]);
   (4, [3; 5; 11; 12]);
   (5, [4; 12]);
   (6, [7; 24]);
   (7, [1; 6; 26]);
   (8, [9; 10; 28; 29; 30]);
   (9, [2; 8; 10; 11]);
   (10, [8; 9; 11; 28; 29; 30]);
   (11, [3; 4; 9; 10]);
   (12, [4; 5; 13; 30]);
   (13, [12; 14; 15; 30; 33]);
   (14, [13; 15; 32; 33]);
   (15, [13; 14; 16; 33]);
   (16, [15; 17; 36]);
   (17, [16; 36]);
   (18, [19; 38; 39]);
   (19, [18; 20; 39; 40; 56; 57]);
   (20, [19; 39; 40; 56; 57]);
   (21, [22; 23; 42]);
   (22, [21; 23; 42; 58; 76; 77]);
   (23, [21; 22; 42]);
   (24, [6; 25]);
   (25, [24; 44]);
   (26, [1; 7; 27; 28; 44]);
   (27, [26; 28; 46]);
   (28, [8; 10; 26; 27; 29; 30]);
   (29, [8; 10; 28; 30; 48; 49]);
   (30, [8; 10; 12; 13; 28; 29; 31; 32; 50]);
   (31, [30; 32; 50; 52]);
   (32, [14; 30; 31; 50]);
   (33, [13; 14; 15; 34; 54]);
   (34, [33; 35; 54]);
   (35, [34; 36; 37; 38]);
   (36, [16; 17; 35; 37; 38]);
   (37, [35; 36; 38; 39; 55; 56]);
   (38, [18; 35; 36; 37; 39]);
   (39, [18; 19; 20; 37; 38; 40; 55; 56; 57]);
   (40, [19; 20; 39; 41; 56; 57]);
   (41, [40; 42]);
   (42, [21; 22; 23; 41; 58; 76; 77]);
   (43, []);
   (44, [25; 26; 59; 60; 79]);
   (45, [46; 47; 48; 61; 62; 79; 80]);
   (46, [27; 45; 47; 48; 61; 62; 79; 80]);
   (47, [45; 46; 48; 61; 62; 79; 80]);
   (48, [29; 45; 46; 47; 49; 61; 62; 79; 80]);
   (49, [29; 48; 64]);
   (50, [30; 31; 32; 51; 66]);
   (51, [50; 52; 66; 67]);
   (52, [31; 51; 53; 54; 67]);
   (53, [52; 54; 67; 68; 84; 86]);
   (54, [33; 34; 52; 53]);
   (55, [37; 39; 56; 68]);
   (56, [19; 20; 37; 39; 40; 55; 57; 69; 70; 71; 72; 73]);
   (57, [19; 20; 39; 40; 56; 69; 70; 71; 72; 73]);
   (58, [22; 42; 73; 76; 77]);
   (59, [44; 60; 79]);
   (60, [44; 59; 78; 79]);
   (61, [45; 46; 47; 48; 62; 79; 80]);
   (62, [45; 46; 47; 48; 61; 63; 79; 80; 82]);
   (63, [62; 64; 65; 66; 82]);
   (64, [49; 63; 65; 66]);
   (65, [63; 64; 66; 83; 84; 99]);
   (66, [50; 51; 63; 64; 65]);
   (67, [51; 52; 53; 68; 84; 86]);
   (68, [53; 55; 67; 84; 86]);
   (69, [56; 57; 70; 71; 72; 73; 102; 103; 127]);
   (70, [56; 57; 69; 71; 72; 73; 87; 103; 128; 129; 144]);
   (71, [56; 57; 69; 70; 72; 73; 87; 88; 104]);
   (72, [56; 57; 69; 70; 71; 73; 88; 89; 105]);
   (73, [56; 57; 58; 69; 70; 71; 72; 74]);
   (74, [73; 75; 90]);
   (75, [74; 76; 90]);
   (76, [22; 42; 58; 75; 77]);
   (77, [22; 42; 58; 76]);
   (78, [60; 79; 96; 97]);
   (79, [44; 45; 46; 47; 48; 59; 60; 61; 62; 78; 80]);
   (80, [45; 46; 47; 48; 61; 62; 79; 81; 97; 98; 117; 118]);
   (81, [80; 97; 98; 117; 118]);
   (82, [62; 63; 83; 98; 120]);
   (83, [65; 82; 84; 98; 99; 120]);
   (84, [53; 65; 67; 68; 83; 86; 99]);
   (85, [86; 100; 101; 102; 124]);
   (86, [53; 67; 68; 84; 85; 101; 102]);
   (87, [70; 71; 103; 104; 128; 129; 144]);
   (88, [71; 72; 89; 104; 105]);
   (89, [72; 88; 90; 91; 105]);
   (90, [74; 75; 89; 91]);
   (91, [89; 90; 92; 107]);
   (92, [91; 93; 107; 109]);
   (93, [92; 94; 109]);
   (94, [93]);
   (95, [96]);
   (96, [78; 95; 97; 114; 115]);
   (97, [78; 80; 81; 96; 116; 117; 118]);
   (98, [80; 81; 82; 83; 118; 120]);
   (99, [65; 83; 84; 100]);
   (100, [85; 99; 120; 122; 124]);
   (101, [85; 86; 102; 126]);
   (102, [69; 85; 86; 101; 127]);
   (103, [69; 70; 87; 127; 128; 129; 144]);
   (104, [71; 87; 88; 129; 130; 145]);
   (105, [72; 88; 89; 106; 130; 131]);
   (106, [105; 107; 108; 130; 131]);
   (107, [91; 92; 106; 108]);
   (108, [106; 107; 132]);
   (109, [92; 93; 110]);
   (110, [109; 111; 132; 134]);
   (111, [110; 132; 134]);
   (112, [113; 135]);
   (113, [112; 114; 135]);
   (114, [96; 113; 115; 137]);
   (115, [96; 114; 116; 137; 138; 139]);
   (116, [97; 115; 117; 137; 138; 139]);
   (117, [80; 81; 97; 116; 118]);
   (118, [80; 81; 97; 98; 117; 119; 151]);
   (119, [118; 121; 151]);
   (120, [82; 83; 98; 100; 122]);
   (121, [119; 123; 140; 153]);
   (122, [100; 120; 123]);
   (123, [121; 122; 140; 153]);
   (124, [85; 100; 125; 126]);
   (125, [124; 126; 155; 156; 182; 183]);
   (126, [101; 124; 125]);
   (127, [69; 102; 103; 128; 143]);
   (128, [70; 87; 103; 127; 129; 143; 144]);
   (129, [70; 87; 103; 104; 128; 144; 145]);
   (130, [104; 105; 106; 131; 145]);
   (131, [105; 106; 130; 133; 146]);
   (132, [108; 110; 111; 134]);
   (133, [131; 134; 146; 147]);
   (134, [110; 111; 132; 133; 147]);
   (135, [112; 113; 148]);
   (136, [138; 148; 149]);
   (137, [114; 115; 116; 138; 139]);
   (138, [115; 116; 136; 137; 139]);
   (139, [115; 116; 137; 138; 150; 164; 175; 176]);
   (140, [121; 123; 153; 154; 170]);
   (141, [155; 170]);
   (142, [143; 156; 157; 158]);
   (143, [127; 128; 142; 158; 159]);
   (144, [70; 87; 103; 128; 129; 159]);
   (145, [104; 129; 130; 160; 161]);
   (146, [131; 133]);
   (147, [133; 134]);
   (148, [135; 136; 149; 162]);
   (149, [136; 148; 163]);
   (150, [139; 151; 164; 175; 176]);
   (151, [118; 119; 150; 152; 153; 166]);
   (152, [151; 153; 166; 167; 179; 180; 191]);
   (153, [121; 123; 140; 151; 152; 166]);
   (154, [140; 168; 170]);
   (155, [125; 141; 156; 170; 182; 183]);
   (156, [125; 142; 155; 157; 158; 182; 183]);
   (157, [142; 156; 158; 171]);
   (158, [142; 143; 156; 157; 159]);
   (159, [143; 144; 158; 172; 173]);
   (160, [145; 161]);
   (161, [145; 160]);
   (162, [148]);
   (163, [149]);
   (164, [139; 150; 174; 175; 176]);
   (165, [166; 176; 177; 189; 190]);
   (166, [151; 152; 153; 165; 176; 189; 190]);
   (167, [152; 178; 179; 180; 191]);
   (168, [154; 169; 180]);
   (169, [168; 170; 180; 181]);
   (170, [140; 141; 154; 155; 169; 181]);
   (171, [157; 183]);
   (172, [159; 185]);
   (173, [159]);
   (174, [164]);
   (175, [139; 150; 164; 176; 188]);
   (176, [139; 150; 164; 165; 166; 175; 189; 190]);
   (177, [165; 178]);
   (178, [167; 177; 179; 189]);
   (179, [152; 167; 178; 180; 191]);
   (180, [152; 167; 168; 169; 179; 191]);
   (181, [169; 170]);
   (182, [125; 155; 156; 183]);
   (183, [125; 155; 156; 171; 182; 185]);
   (184, [192; 193]);
   (185, [172; 183; 186; 187]);
   (186, [185; 187; 193]);
   (187, [185; 186; 195]);
   (188, [175]);
   (189, [165; 166; 176; 178; 190]);
   (190, [165; 166; 176; 189]);
   (191, [152; 167; 179; 180]);
   (192, [184]);
   (193, [184; 186; 194]);
   (194, [193]);
   (195, [187])]