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

(* As Dienes' CSV mostly describes the visual connections between different
   nodes on the board (Circle -> Square -> Circle/Square) and not abstract
   edges (regular move, or carriage move) that can be traversed by Jack, we
   need to handle this logic separately.

   This file summarizes all possible carriage moves that can be made from
   each circle, as infered from the Dienes' step data, and the infered
   regular move data.

   Note: Each edge is listed only only once - for example, once Circle 1
   -> Circle 2 is listed, Circle 2 -> Circle 1 is not listed. It follows
  that each circle only connects to circles of higher index. *) 


let carriages =
  [(Circle 1, [Circle 79; Circle 59; Circle 48; Circle 47; Circle 46; Circle 45; Circle 44; Circle 43; Circle 29; Circle 28; Circle 27; Circle 26; Circle 25; Circle 24; Circle 11; Circle 10; Circle 9; Circle 8; Circle 7; Circle 6; Circle 3; Circle 2]);
   (Circle 2, [Circle 79; Circle 48; Circle 47; Circle 46; Circle 45; Circle 44; Circle 30; Circle 29; Circle 28; Circle 27; Circle 26; Circle 25; Circle 24; Circle 12; Circle 11; Circle 10; Circle 9; Circle 8; Circle 7; Circle 6; Circle 5; Circle 4; Circle 3]);
   (Circle 3, [Circle 30; Circle 28; Circle 26; Circle 17; Circle 16; Circle 15; Circle 13; Circle 12; Circle 11; Circle 10; Circle 9; Circle 8; Circle 5; Circle 4]);
   (Circle 4, [Circle 66; Circle 64; Circle 50; Circle 49; Circle 30; Circle 29; Circle 17; Circle 16; Circle 15; Circle 14; Circle 13; Circle 12; Circle 11; Circle 10; Circle 9; Circle 8; Circle 5]);
   (Circle 5, [Circle 38; Circle 36; Circle 35; Circle 33; Circle 30; Circle 18; Circle 17; Circle 16; Circle 15; Circle 14; Circle 13; Circle 12; Circle 11; Circle 10; Circle 9]);
   (Circle 6, [Circle 79; Circle 59; Circle 46; Circle 44; Circle 43; Circle 28; Circle 27; Circle 26; Circle 25; Circle 24; Circle 9; Circle 8; Circle 7]);
   (Circle 7, [Circle 79; Circle 59; Circle 46; Circle 44; Circle 43; Circle 28; Circle 27; Circle 26; Circle 25; Circle 24; Circle 9; Circle 8]);
   (Circle 8, [Circle 79; Circle 48; Circle 47; Circle 46; Circle 45; Circle 44; Circle 30; Circle 29; Circle 28; Circle 27; Circle 26; Circle 25; Circle 24; Circle 12; Circle 11; Circle 10; Circle 9]);
   (Circle 9, [Circle 79; Circle 48; Circle 47; Circle 46; Circle 45; Circle 44; Circle 30; Circle 29; Circle 28; Circle 27; Circle 26; Circle 25; Circle 24; Circle 12; Circle 11; Circle 10]);
   (Circle 10, [Circle 66; Circle 64; Circle 50; Circle 49; Circle 30; Circle 29; Circle 28; Circle 26; Circle 17; Circle 16; Circle 15; Circle 14; Circle 13; Circle 12; Circle 11]);
   (Circle 11, [Circle 66; Circle 64; Circle 50; Circle 49; Circle 30; Circle 29; Circle 28; Circle 26; Circle 17; Circle 16; Circle 15; Circle 14; Circle 13; Circle 12]);
   (Circle 12, [Circle 66; Circle 64; Circle 50; Circle 49; Circle 38; Circle 36; Circle 35; Circle 33; Circle 30; Circle 29; Circle 18; Circle 17; Circle 16; Circle 15; Circle 14; Circle 13]);
   (Circle 13, [Circle 66; Circle 64; Circle 54; Circle 52; Circle 50; Circle 49; Circle 38; Circle 36; Circle 35; Circle 33; Circle 32; Circle 31; Circle 30; Circle 29; Circle 18; Circle 17; Circle 16; Circle 15; Circle 14]);
   (Circle 14, [Circle 68; Circle 67; Circle 66; Circle 64; Circle 55; Circle 54; Circle 53; Circle 52; Circle 51; Circle 50; Circle 49; Circle 37; Circle 36; Circle 35; Circle 34; Circle 33; Circle 32; Circle 31; Circle 30; Circle 29; Circle 17; Circle 16; Circle 15]);
   (Circle 15, [Circle 68; Circle 55; Circle 54; Circle 53; Circle 52; Circle 38; Circle 37; Circle 36; Circle 35; Circle 34; Circle 33; Circle 32; Circle 31; Circle 30; Circle 18; Circle 17; Circle 16]);
   (Circle 16, [Circle 68; Circle 55; Circle 54; Circle 53; Circle 52; Circle 38; Circle 37; Circle 36; Circle 35; Circle 34; Circle 33; Circle 32; Circle 31; Circle 30; Circle 18; Circle 17]);
   (Circle 17, [Circle 39; Circle 38; Circle 37; Circle 36; Circle 35; Circle 33; Circle 30; Circle 20; Circle 19; Circle 18]);
   (Circle 18, [Circle 56; Circle 42; Circle 41; Circle 40; Circle 39; Circle 38; Circle 37; Circle 36; Circle 35; Circle 33; Circle 21; Circle 20; Circle 19]);
   (Circle 19, [Circle 56; Circle 42; Circle 41; Circle 40; Circle 39; Circle 38; Circle 37; Circle 36; Circle 21; Circle 20]);
   (Circle 20, [Circle 73; Circle 58; Circle 57; Circle 42; Circle 41; Circle 40; Circle 39; Circle 38; Circle 36; Circle 23; Circle 22; Circle 21]);
   (Circle 21, [Circle 77; Circle 73; Circle 58; Circle 57; Circle 42; Circle 41; Circle 40; Circle 22]);
   (Circle 22, [Circle 94; Circle 93; Circle 92; Circle 91; Circle 90; Circle 77; Circle 76; Circle 75; Circle 73; Circle 58; Circle 57; Circle 41; Circle 40; Circle 23]);
   (Circle 23, [Circle 94; Circle 93; Circle 92; Circle 91; Circle 90; Circle 77; Circle 76; Circle 75; Circle 42; Circle 41; Circle 40]);
   (Circle 24, [Circle 96; Circle 95; Circle 79; Circle 78; Circle 60; Circle 59; Circle 46; Circle 44; Circle 43; Circle 28; Circle 27; Circle 26; Circle 25]);
   (Circle 25, [Circle 96; Circle 95; Circle 79; Circle 78; Circle 60; Circle 59; Circle 46; Circle 44; Circle 43; Circle 28; Circle 27; Circle 26]);
   (Circle 26, [Circle 97; Circle 80; Circle 79; Circle 78; Circle 60; Circle 59; Circle 48; Circle 47; Circle 46; Circle 45; Circle 44; Circle 43; Circle 29; Circle 28; Circle 27]);
   (Circle 27, [Circle 97; Circle 80; Circle 79; Circle 78; Circle 66; Circle 64; Circle 63; Circle 62; Circle 61; Circle 60; Circle 59; Circle 50; Circle 49; Circle 48; Circle 47; Circle 46; Circle 45; Circle 44; Circle 43; Circle 30; Circle 29; Circle 28]);
   (Circle 28, [Circle 79; Circle 66; Circle 64; Circle 63; Circle 62; Circle 61; Circle 50; Circle 49; Circle 48; Circle 47; Circle 46; Circle 45; Circle 44; Circle 30; Circle 29]);
   (Circle 29, [Circle 84; Circle 79; Circle 67; Circle 66; Circle 65; Circle 64; Circle 63; Circle 62; Circle 61; Circle 52; Circle 51; Circle 50; Circle 49; Circle 48; Circle 47; Circle 46; Circle 45; Circle 44; Circle 31; Circle 30]);
   (Circle 30, [Circle 84; Circle 67; Circle 66; Circle 65; Circle 64; Circle 63; Circle 62; Circle 54; Circle 52; Circle 51; Circle 50; Circle 49; Circle 48; Circle 47; Circle 46; Circle 45; Circle 33; Circle 32; Circle 31]);
   (Circle 31, [Circle 84; Circle 68; Circle 67; Circle 66; Circle 65; Circle 64; Circle 55; Circle 54; Circle 53; Circle 52; Circle 51; Circle 50; Circle 49; Circle 37; Circle 36; Circle 35; Circle 34; Circle 33; Circle 32]);
   (Circle 32, [Circle 68; Circle 67; Circle 55; Circle 54; Circle 53; Circle 52; Circle 51; Circle 50; Circle 37; Circle 36; Circle 35; Circle 34; Circle 33]);
   (Circle 33, [Circle 68; Circle 67; Circle 55; Circle 54; Circle 53; Circle 52; Circle 51; Circle 50; Circle 38; Circle 37; Circle 36; Circle 35; Circle 34]);
   (Circle 34, [Circle 86; Circle 68; Circle 67; Circle 56; Circle 55; Circle 54; Circle 53; Circle 52; Circle 39; Circle 38; Circle 37; Circle 36; Circle 35]);
   (Circle 35, [Circle 86; Circle 68; Circle 67; Circle 56; Circle 55; Circle 54; Circle 53; Circle 52; Circle 39; Circle 38; Circle 37; Circle 36]);
   (Circle 36, [Circle 68; Circle 55; Circle 54; Circle 53; Circle 52; Circle 39; Circle 38; Circle 37]);
   (Circle 37, [Circle 86; Circle 68; Circle 67; Circle 56; Circle 55; Circle 54; Circle 53; Circle 52; Circle 39; Circle 38]);
   (Circle 38, [Circle 68; Circle 56; Circle 55; Circle 54; Circle 53; Circle 39]);
   (Circle 39, [Circle 86; Circle 68; Circle 57; Circle 55; Circle 54; Circle 53]);
   (Circle 40, [Circle 90; Circle 89; Circle 76; Circle 75; Circle 74; Circle 73; Circle 72; Circle 58; Circle 57; Circle 56; Circle 42; Circle 41]);
   (Circle 41, [Circle 90; Circle 89; Circle 76; Circle 75; Circle 74; Circle 73; Circle 72; Circle 58; Circle 57; Circle 56; Circle 42]);
   (Circle 42, [Circle 90; Circle 89; Circle 77; Circle 76; Circle 75; Circle 74; Circle 73; Circle 72; Circle 58; Circle 57; Circle 56]);
   (Circle 43, [Circle 96; Circle 95; Circle 79; Circle 78; Circle 60; Circle 59; Circle 46; Circle 44]);
   (Circle 44, [Circle 97; Circle 96; Circle 95; Circle 80; Circle 79; Circle 78; Circle 60; Circle 59; Circle 48; Circle 47; Circle 46; Circle 45]);
   (Circle 45, [Circle 79; Circle 66; Circle 64; Circle 63; Circle 62; Circle 61; Circle 50; Circle 49; Circle 48; Circle 47; Circle 46]);
   (Circle 46, [Circle 97; Circle 80; Circle 79; Circle 78; Circle 66; Circle 64; Circle 63; Circle 62; Circle 61; Circle 60; Circle 59; Circle 50; Circle 49; Circle 48; Circle 47]);
   (Circle 47, [Circle 79; Circle 66; Circle 64; Circle 63; Circle 62; Circle 61; Circle 50; Circle 49; Circle 48]);
   (Circle 48, [Circle 98; Circle 83; Circle 82; Circle 80; Circle 79; Circle 66; Circle 65; Circle 64; Circle 63; Circle 62; Circle 61; Circle 50; Circle 49]);
   (Circle 49, [Circle 98; Circle 84; Circle 83; Circle 82; Circle 80; Circle 67; Circle 66; Circle 65; Circle 64; Circle 63; Circle 62; Circle 52; Circle 51; Circle 50]);
   (Circle 50, [Circle 84; Circle 67; Circle 66; Circle 65; Circle 64; Circle 63; Circle 62; Circle 54; Circle 53; Circle 52; Circle 51]);
   (Circle 51, [Circle 100; Circle 99; Circle 86; Circle 85; Circle 84; Circle 83; Circle 82; Circle 67; Circle 66; Circle 65; Circle 64; Circle 63; Circle 54; Circle 53; Circle 52]);
   (Circle 52, [Circle 84; Circle 68; Circle 67; Circle 66; Circle 65; Circle 64; Circle 55; Circle 54; Circle 53]);
   (Circle 53, [Circle 86; Circle 84; Circle 68; Circle 67; Circle 66; Circle 65; Circle 56; Circle 55; Circle 54]);
   (Circle 54, [Circle 86; Circle 68; Circle 67; Circle 56; Circle 55]);
   (Circle 55, [Circle 100; Circle 99; Circle 86; Circle 85; Circle 84; Circle 68; Circle 67; Circle 57; Circle 56]);
   (Circle 56, [Circle 100; Circle 99; Circle 86; Circle 85; Circle 84; Circle 73; Circle 68; Circle 58]);
   (Circle 57, [Circle 90; Circle 89; Circle 86; Circle 76; Circle 75; Circle 74; Circle 73; Circle 72; Circle 68; Circle 58]);
   (Circle 58, [Circle 94; Circle 93; Circle 92; Circle 91; Circle 90; Circle 89; Circle 77; Circle 76; Circle 75; Circle 74; Circle 73; Circle 72]);
   (Circle 59, [Circle 116; Circle 115; Circle 114; Circle 113; Circle 112; Circle 97; Circle 96; Circle 95; Circle 80; Circle 79; Circle 78; Circle 60]);
   (Circle 60, [Circle 116; Circle 115; Circle 114; Circle 113; Circle 112; Circle 97; Circle 96; Circle 95; Circle 80; Circle 79; Circle 78]);
   (Circle 62, [Circle 123; Circle 122; Circle 121; Circle 120; Circle 119; Circle 118; Circle 98; Circle 97; Circle 83; Circle 82; Circle 81; Circle 80; Circle 79; Circle 78; Circle 66; Circle 65; Circle 64; Circle 63]);
   (Circle 63, [Circle 120; Circle 100; Circle 99; Circle 98; Circle 84; Circle 83; Circle 82; Circle 80; Circle 67; Circle 66; Circle 65; Circle 64]);
   (Circle 64, [Circle 98; Circle 84; Circle 83; Circle 82; Circle 80; Circle 67; Circle 66; Circle 65]);
   (Circle 65, [Circle 120; Circle 100; Circle 99; Circle 98; Circle 86; Circle 85; Circle 84; Circle 83; Circle 82; Circle 80; Circle 67; Circle 66]);
   (Circle 66, [Circle 100; Circle 99; Circle 86; Circle 85; Circle 84; Circle 83; Circle 82; Circle 67]);
   (Circle 67, [Circle 100; Circle 99; Circle 86; Circle 85; Circle 84; Circle 83; Circle 82; Circle 68]);
   (Circle 68, [Circle 100; Circle 99; Circle 86; Circle 85; Circle 84]);
   (Circle 69, [Circle 156; Circle 143; Circle 142; Circle 128; Circle 127; Circle 126; Circle 125; Circle 103; Circle 102; Circle 101; Circle 87; Circle 71; Circle 70]);
   (Circle 70, [Circle 129; Circle 128; Circle 127; Circle 104; Circle 103; Circle 102; Circle 88; Circle 87; Circle 72; Circle 71]);
   (Circle 71, [Circle 145; Circle 130; Circle 129; Circle 105; Circle 104; Circle 103; Circle 90; Circle 89; Circle 88; Circle 87; Circle 74; Circle 73; Circle 72]);
   (Circle 72, [Circle 130; Circle 107; Circle 106; Circle 105; Circle 104; Circle 94; Circle 93; Circle 92; Circle 91; Circle 90; Circle 89; Circle 88; Circle 87; Circle 77; Circle 76; Circle 75; Circle 74; Circle 73]);
   (Circle 73, [Circle 107; Circle 106; Circle 105; Circle 94; Circle 93; Circle 92; Circle 91; Circle 90; Circle 89; Circle 88; Circle 77; Circle 76; Circle 75; Circle 74]);
   (Circle 74, [Circle 107; Circle 106; Circle 105; Circle 94; Circle 93; Circle 92; Circle 91; Circle 90; Circle 89; Circle 88; Circle 77; Circle 76; Circle 75]);
   (Circle 75, [Circle 132; Circle 111; Circle 110; Circle 109; Circle 108; Circle 107; Circle 106; Circle 105; Circle 94; Circle 93; Circle 92; Circle 91; Circle 90; Circle 89; Circle 77; Circle 76]);
   (Circle 76, [Circle 132; Circle 111; Circle 110; Circle 109; Circle 108; Circle 107; Circle 106; Circle 105; Circle 94; Circle 93; Circle 92; Circle 91; Circle 90; Circle 89; Circle 77]);
   (Circle 77, [Circle 132; Circle 111; Circle 110; Circle 109; Circle 108; Circle 107; Circle 106; Circle 105; Circle 94; Circle 93; Circle 92; Circle 91; Circle 90; Circle 89]);
   (Circle 78, [Circle 117; Circle 116; Circle 115; Circle 114; Circle 113; Circle 112; Circle 98; Circle 97; Circle 96; Circle 95; Circle 82; Circle 81; Circle 80; Circle 79]);
   (Circle 79, [Circle 117; Circle 116; Circle 115; Circle 98; Circle 97; Circle 96; Circle 95; Circle 82; Circle 81; Circle 80]);
   (Circle 80, [Circle 123; Circle 122; Circle 121; Circle 120; Circle 119; Circle 118; Circle 117; Circle 116; Circle 115; Circle 98; Circle 97; Circle 96; Circle 95; Circle 83; Circle 82]);
   (Circle 81, [Circle 151; Circle 150; Circle 139; Circle 123; Circle 122; Circle 121; Circle 120; Circle 119; Circle 117; Circle 116; Circle 98; Circle 97; Circle 82]);
   (Circle 82, [Circle 123; Circle 122; Circle 121; Circle 120; Circle 119; Circle 118; Circle 100; Circle 99; Circle 98; Circle 97; Circle 84; Circle 83]);
   (Circle 83, [Circle 170; Circle 155; Circle 141; Circle 140; Circle 125; Circle 124; Circle 123; Circle 122; Circle 121; Circle 120; Circle 119; Circle 118; Circle 100; Circle 99; Circle 98; Circle 86; Circle 85; Circle 84]);
   (Circle 84, [Circle 170; Circle 155; Circle 141; Circle 140; Circle 125; Circle 124; Circle 123; Circle 122; Circle 120; Circle 101; Circle 100; Circle 99; Circle 86; Circle 85]);
   (Circle 85, [Circle 170; Circle 156; Circle 155; Circle 143; Circle 142; Circle 141; Circle 140; Circle 127; Circle 126; Circle 125; Circle 124; Circle 123; Circle 122; Circle 120; Circle 102; Circle 101; Circle 100; Circle 99; Circle 86]);
   (Circle 86, [Circle 170; Circle 155; Circle 141; Circle 140; Circle 125; Circle 124; Circle 123; Circle 122; Circle 120; Circle 101; Circle 100; Circle 99]);
   (Circle 87, [Circle 145; Circle 144; Circle 130; Circle 129; Circle 105; Circle 104; Circle 103; Circle 88]);
   (Circle 88, [Circle 161; Circle 146; Circle 145; Circle 131; Circle 130; Circle 129; Circle 107; Circle 106; Circle 105; Circle 104; Circle 91; Circle 90; Circle 89]);
   (Circle 89, [Circle 134; Circle 133; Circle 132; Circle 131; Circle 130; Circle 110; Circle 109; Circle 108; Circle 107; Circle 106; Circle 105; Circle 104; Circle 94; Circle 93; Circle 92; Circle 91; Circle 90]);
   (Circle 90, [Circle 132; Circle 111; Circle 110; Circle 109; Circle 108; Circle 107; Circle 106; Circle 105; Circle 94; Circle 93; Circle 92; Circle 91]);
   (Circle 91, [Circle 134; Circle 133; Circle 132; Circle 131; Circle 130; Circle 111; Circle 110; Circle 109; Circle 108; Circle 107; Circle 106; Circle 105; Circle 104; Circle 94; Circle 93; Circle 92]);
   (Circle 92, [Circle 134; Circle 133; Circle 132; Circle 131; Circle 111; Circle 110; Circle 109; Circle 108; Circle 107; Circle 106; Circle 105; Circle 94; Circle 93]);
   (Circle 93, [Circle 147; Circle 134; Circle 132; Circle 111; Circle 110; Circle 109; Circle 108; Circle 107; Circle 106; Circle 105; Circle 94]);
   (Circle 94, [Circle 147; Circle 134; Circle 132; Circle 111; Circle 110; Circle 109; Circle 108; Circle 107; Circle 106; Circle 105]);
   (Circle 95, [Circle 162; Circle 148; Circle 138; Circle 137; Circle 136; Circle 135; Circle 116; Circle 115; Circle 114; Circle 113; Circle 112; Circle 97; Circle 96]);
   (Circle 96, [Circle 162; Circle 151; Circle 150; Circle 148; Circle 139; Circle 138; Circle 137; Circle 136; Circle 135; Circle 118; Circle 117; Circle 116; Circle 115; Circle 114; Circle 113; Circle 112; Circle 97]);
   (Circle 97, [Circle 151; Circle 150; Circle 139; Circle 137; Circle 118; Circle 117; Circle 116; Circle 115; Circle 114; Circle 113; Circle 112; Circle 98]);
   (Circle 98, [Circle 170; Circle 155; Circle 153; Circle 151; Circle 150; Circle 141; Circle 140; Circle 139; Circle 125; Circle 124; Circle 123; Circle 122; Circle 121; Circle 120; Circle 119; Circle 118; Circle 117; Circle 116; Circle 100; Circle 99]);
   (Circle 99, [Circle 170; Circle 155; Circle 141; Circle 140; Circle 125; Circle 124; Circle 123; Circle 122; Circle 121; Circle 120; Circle 119; Circle 118; Circle 101; Circle 100]);
   (Circle 100, [Circle 182; Circle 181; Circle 180; Circle 170; Circle 169; Circle 168; Circle 156; Circle 155; Circle 154; Circle 153; Circle 152; Circle 143; Circle 142; Circle 141; Circle 140; Circle 127; Circle 126; Circle 125; Circle 124; Circle 123; Circle 122; Circle 121; Circle 120; Circle 119; Circle 118; Circle 102; Circle 101]);
   (Circle 101, [Circle 183; Circle 171; Circle 170; Circle 158; Circle 157; Circle 156; Circle 155; Circle 144; Circle 143; Circle 142; Circle 141; Circle 140; Circle 128; Circle 127; Circle 126; Circle 125; Circle 124; Circle 123; Circle 122; Circle 103; Circle 102]);
   (Circle 102, [Circle 183; Circle 171; Circle 170; Circle 158; Circle 157; Circle 156; Circle 155; Circle 144; Circle 143; Circle 142; Circle 141; Circle 140; Circle 128; Circle 127; Circle 126; Circle 125; Circle 124; Circle 123; Circle 122; Circle 103]);
   (Circle 103, [Circle 156; Circle 144; Circle 143; Circle 142; Circle 128; Circle 127; Circle 126; Circle 125]);
   (Circle 104, [Circle 161; Circle 160; Circle 146; Circle 145; Circle 144; Circle 131; Circle 130; Circle 129; Circle 107; Circle 106; Circle 105]);
   (Circle 105, [Circle 161; Circle 146; Circle 145; Circle 134; Circle 133; Circle 132; Circle 131; Circle 130; Circle 129; Circle 110; Circle 109; Circle 108; Circle 107; Circle 106]);
   (Circle 106, [Circle 161; Circle 147; Circle 146; Circle 145; Circle 134; Circle 133; Circle 132; Circle 131; Circle 130; Circle 111; Circle 110; Circle 109; Circle 108; Circle 107]);
   (Circle 107, [Circle 134; Circle 133; Circle 132; Circle 131; Circle 130; Circle 111; Circle 110; Circle 109; Circle 108]);
   (Circle 108, [Circle 161; Circle 147; Circle 146; Circle 145; Circle 134; Circle 133; Circle 132; Circle 131; Circle 130; Circle 111; Circle 110; Circle 109]);
   (Circle 109, [Circle 147; Circle 134; Circle 133; Circle 132; Circle 131; Circle 111; Circle 110]);
   (Circle 110, [Circle 147; Circle 134; Circle 133; Circle 132; Circle 131; Circle 111]);
   (Circle 111, [Circle 147; Circle 146; Circle 134; Circle 133; Circle 132; Circle 131]);
   (Circle 112, [Circle 163; Circle 162; Circle 149; Circle 148; Circle 138; Circle 137; Circle 136; Circle 135; Circle 116; Circle 115; Circle 114; Circle 113]);
   (Circle 113, [Circle 174; Circle 164; Circle 163; Circle 162; Circle 149; Circle 148; Circle 139; Circle 138; Circle 137; Circle 136; Circle 135; Circle 116; Circle 115; Circle 114]);
   (Circle 114, [Circle 174; Circle 164; Circle 163; Circle 162; Circle 149; Circle 148; Circle 139; Circle 138; Circle 137; Circle 136; Circle 135; Circle 116; Circle 115]);
   (Circle 115, [Circle 151; Circle 150; Circle 148; Circle 139; Circle 138; Circle 137; Circle 136; Circle 135; Circle 118; Circle 117; Circle 116]);
   (Circle 116, [Circle 176; Circle 174; Circle 166; Circle 164; Circle 163; Circle 153; Circle 151; Circle 150; Circle 149; Circle 139; Circle 138; Circle 137; Circle 136; Circle 123; Circle 122; Circle 121; Circle 120; Circle 119; Circle 118; Circle 117]);
   (Circle 117, [Circle 176; Circle 174; Circle 166; Circle 164; Circle 163; Circle 153; Circle 151; Circle 150; Circle 149; Circle 139; Circle 138; Circle 136; Circle 123; Circle 122; Circle 121; Circle 120; Circle 119; Circle 118]);
   (Circle 118, [Circle 176; Circle 174; Circle 170; Circle 166; Circle 164; Circle 163; Circle 155; Circle 153; Circle 151; Circle 150; Circle 149; Circle 141; Circle 140; Circle 139; Circle 138; Circle 136; Circle 125; Circle 124; Circle 123; Circle 122; Circle 121; Circle 120; Circle 119]);
   (Circle 119, [Circle 180; Circle 176; Circle 170; Circle 168; Circle 166; Circle 155; Circle 154; Circle 153; Circle 152; Circle 151; Circle 150; Circle 141; Circle 140; Circle 139; Circle 125; Circle 124; Circle 123; Circle 122; Circle 121; Circle 120]);
   (Circle 120, [Circle 170; Circle 155; Circle 153; Circle 151; Circle 150; Circle 141; Circle 140; Circle 139; Circle 125; Circle 124; Circle 123; Circle 122; Circle 121]);
   (Circle 121, [Circle 180; Circle 176; Circle 170; Circle 168; Circle 166; Circle 155; Circle 154; Circle 153; Circle 152; Circle 151; Circle 150; Circle 141; Circle 140; Circle 139; Circle 125; Circle 124; Circle 123; Circle 122]);
   (Circle 122, [Circle 182; Circle 181; Circle 180; Circle 170; Circle 169; Circle 168; Circle 156; Circle 155; Circle 154; Circle 153; Circle 152; Circle 151; Circle 150; Circle 143; Circle 142; Circle 141; Circle 140; Circle 139; Circle 127; Circle 126; Circle 125; Circle 124; Circle 123]);
   (Circle 123, [Circle 182; Circle 181; Circle 180; Circle 170; Circle 169; Circle 168; Circle 156; Circle 155; Circle 154; Circle 153; Circle 152; Circle 151; Circle 150; Circle 143; Circle 142; Circle 141; Circle 140; Circle 139; Circle 127; Circle 126; Circle 125; Circle 124]);
   (Circle 124, [Circle 182; Circle 181; Circle 180; Circle 170; Circle 169; Circle 168; Circle 156; Circle 155; Circle 154; Circle 153; Circle 152; Circle 143; Circle 142; Circle 141; Circle 140; Circle 127; Circle 126; Circle 125]);
   (Circle 125, [Circle 183; Circle 182; Circle 181; Circle 180; Circle 171; Circle 170; Circle 169; Circle 168; Circle 158; Circle 157; Circle 156; Circle 155; Circle 154; Circle 153; Circle 152; Circle 144; Circle 143; Circle 142; Circle 141; Circle 140; Circle 128; Circle 127; Circle 126]);
   (Circle 126, [Circle 183; Circle 171; Circle 170; Circle 158; Circle 157; Circle 156; Circle 155; Circle 144; Circle 143; Circle 142; Circle 141; Circle 140; Circle 128; Circle 127]);
   (Circle 127, [Circle 183; Circle 171; Circle 170; Circle 158; Circle 157; Circle 156; Circle 155; Circle 144; Circle 143; Circle 142; Circle 141; Circle 140; Circle 128]);
   (Circle 128, [Circle 158; Circle 156; Circle 145; Circle 144; Circle 143; Circle 142; Circle 129]);
   (Circle 129, [Circle 161; Circle 160; Circle 146; Circle 145; Circle 144; Circle 143; Circle 131; Circle 130]);
   (Circle 130, [Circle 161; Circle 160; Circle 147; Circle 146; Circle 145; Circle 144; Circle 134; Circle 133; Circle 132; Circle 131]);
   (Circle 131, [Circle 161; Circle 160; Circle 147; Circle 146; Circle 145; Circle 144; Circle 134; Circle 133; Circle 132]);
   (Circle 132, [Circle 161; Circle 147; Circle 146; Circle 145; Circle 134; Circle 133]);
   (Circle 133, [Circle 161; Circle 147; Circle 146; Circle 145; Circle 134]);
   (Circle 134, [Circle 161; Circle 147; Circle 146; Circle 145]);
   (Circle 135, [Circle 174; Circle 164; Circle 163; Circle 162; Circle 149; Circle 148; Circle 139; Circle 138; Circle 137; Circle 136]);
   (Circle 136, [Circle 188; Circle 175; Circle 174; Circle 164; Circle 163; Circle 162; Circle 151; Circle 150; Circle 149; Circle 148; Circle 139; Circle 138; Circle 137]);
   (Circle 137, [Circle 174; Circle 164; Circle 163; Circle 162; Circle 149; Circle 148; Circle 139; Circle 138]);
   (Circle 138, [Circle 188; Circle 175; Circle 174; Circle 164; Circle 163; Circle 162; Circle 151; Circle 150; Circle 149; Circle 148; Circle 139]);
   (Circle 139, [Circle 188; Circle 176; Circle 175; Circle 174; Circle 166; Circle 164; Circle 163; Circle 162; Circle 153; Circle 151; Circle 150; Circle 149; Circle 148]);
   (Circle 140, [Circle 191; Circle 182; Circle 181; Circle 180; Circle 178; Circle 177; Circle 170; Circle 169; Circle 168; Circle 167; Circle 166; Circle 165; Circle 156; Circle 155; Circle 154; Circle 153; Circle 152; Circle 151; Circle 143; Circle 142; Circle 141]);
   (Circle 141, [Circle 182; Circle 181; Circle 180; Circle 170; Circle 169; Circle 168; Circle 156; Circle 155; Circle 154; Circle 153; Circle 152; Circle 143; Circle 142]);
   (Circle 142, [Circle 185; Circle 183; Circle 172; Circle 171; Circle 170; Circle 159; Circle 158; Circle 157; Circle 156; Circle 155; Circle 144; Circle 143]);
   (Circle 143, [Circle 185; Circle 183; Circle 172; Circle 171; Circle 170; Circle 159; Circle 158; Circle 157; Circle 156; Circle 155; Circle 145; Circle 144]);
   (Circle 144, [Circle 161; Circle 160; Circle 158; Circle 156; Circle 146; Circle 145]);
   (Circle 145, [Circle 173; Circle 161; Circle 160; Circle 159; Circle 147; Circle 146]);
   (Circle 146, [Circle 161; Circle 160; Circle 147]);
   (Circle 147, [Circle 161]);
   (Circle 148, [Circle 174; Circle 164; Circle 163; Circle 162; Circle 149]);
   (Circle 149, [Circle 188; Circle 175; Circle 174; Circle 164; Circle 163; Circle 162; Circle 151; Circle 150]);
   (Circle 150, [Circle 190; Circle 188; Circle 178; Circle 177; Circle 176; Circle 175; Circle 174; Circle 167; Circle 166; Circle 165; Circle 164; Circle 163; Circle 153; Circle 152; Circle 151]);
   (Circle 151, [Circle 190; Circle 188; Circle 180; Circle 178; Circle 177; Circle 176; Circle 175; Circle 174; Circle 168; Circle 167; Circle 166; Circle 165; Circle 164; Circle 163; Circle 154; Circle 153; Circle 152]);
   (Circle 152, [Circle 191; Circle 189; Circle 181; Circle 180; Circle 179; Circle 178; Circle 177; Circle 176; Circle 170; Circle 169; Circle 168; Circle 167; Circle 166; Circle 165; Circle 155; Circle 154; Circle 153]);
   (Circle 153, [Circle 191; Circle 181; Circle 180; Circle 178; Circle 177; Circle 176; Circle 170; Circle 169; Circle 168; Circle 167; Circle 166; Circle 165; Circle 155; Circle 154]);
   (Circle 154, [Circle 191; Circle 182; Circle 181; Circle 180; Circle 178; Circle 177; Circle 170; Circle 169; Circle 168; Circle 167; Circle 166; Circle 165; Circle 155]);
   (Circle 155, [Circle 193; Circle 192; Circle 191; Circle 186; Circle 185; Circle 184; Circle 183; Circle 182; Circle 181; Circle 180; Circle 170; Circle 169; Circle 168; Circle 156]);
   (Circle 156, [Circle 193; Circle 192; Circle 186; Circle 185; Circle 184; Circle 183; Circle 182; Circle 172; Circle 171; Circle 170; Circle 159; Circle 158; Circle 157]);
   (Circle 157, [Circle 195; Circle 193; Circle 192; Circle 187; Circle 186; Circle 185; Circle 184; Circle 183; Circle 182; Circle 173; Circle 172; Circle 171; Circle 160; Circle 159; Circle 158]);
   (Circle 158, [Circle 195; Circle 193; Circle 192; Circle 187; Circle 186; Circle 185; Circle 184; Circle 183; Circle 182; Circle 173; Circle 172; Circle 171; Circle 160; Circle 159]);
   (Circle 159, [Circle 195; Circle 194; Circle 193; Circle 192; Circle 187; Circle 186; Circle 185; Circle 184; Circle 183; Circle 182; Circle 173; Circle 172; Circle 171; Circle 161; Circle 160]);
   (Circle 160, [Circle 195; Circle 187; Circle 185; Circle 183; Circle 173; Circle 172; Circle 171; Circle 161]);
   (Circle 161, [Circle 173]);
   (Circle 162, [Circle 174; Circle 164; Circle 163]);
   (Circle 163, [Circle 188; Circle 175; Circle 174; Circle 164]);
   (Circle 164, [Circle 190; Circle 188; Circle 176; Circle 175; Circle 174]);
   (Circle 165, [Circle 189; Circle 180; Circle 179; Circle 178; Circle 177; Circle 176; Circle 168; Circle 167; Circle 166]);
   (Circle 166, [Circle 190; Circle 189; Circle 188; Circle 180; Circle 179; Circle 178; Circle 177; Circle 176; Circle 175; Circle 168; Circle 167]);
   (Circle 167, [Circle 189; Circle 180; Circle 179; Circle 178; Circle 177; Circle 176; Circle 168]);
   (Circle 168, [Circle 191; Circle 182; Circle 181; Circle 180; Circle 178; Circle 177; Circle 170; Circle 169]);
   (Circle 169, [Circle 191; Circle 190; Circle 182; Circle 181; Circle 180; Circle 170]);
   (Circle 170, [Circle 193; Circle 192; Circle 191; Circle 186; Circle 185; Circle 184; Circle 183; Circle 182; Circle 181; Circle 180]);
   (Circle 171, [Circle 195; Circle 193; Circle 192; Circle 187; Circle 186; Circle 185; Circle 184; Circle 183; Circle 182; Circle 173; Circle 172]);
   (Circle 172, [Circle 195; Circle 194; Circle 193; Circle 192; Circle 187; Circle 186; Circle 185; Circle 184; Circle 183; Circle 182; Circle 173]);
   (Circle 173, [Circle 195; Circle 194; Circle 193; Circle 192; Circle 187; Circle 186; Circle 185; Circle 184; Circle 183; Circle 182]);
   (Circle 174, [Circle 190; Circle 188; Circle 176; Circle 175]);
   (Circle 175, [Circle 191; Circle 190; Circle 188; Circle 176]);
   (Circle 176, [Circle 191; Circle 190; Circle 188; Circle 178; Circle 177]);
   (Circle 177, [Circle 189; Circle 180; Circle 179; Circle 178]);
   (Circle 178, [Circle 189; Circle 180; Circle 179]);
   (Circle 179, [Circle 189]);
   (Circle 180, [Circle 191; Circle 190; Circle 182; Circle 181]);
   (Circle 181, [Circle 193; Circle 192; Circle 191; Circle 190; Circle 186; Circle 185; Circle 184; Circle 183; Circle 182]);
   (Circle 182, [Circle 195; Circle 194; Circle 193; Circle 192; Circle 191; Circle 187; Circle 186; Circle 185; Circle 184; Circle 183]);
   (Circle 183, [Circle 195; Circle 194; Circle 193; Circle 192; Circle 187; Circle 186; Circle 185; Circle 184]);
   (Circle 184, [Circle 195; Circle 194; Circle 193; Circle 192; Circle 187; Circle 186; Circle 185]);
   (Circle 185, [Circle 195; Circle 194; Circle 193; Circle 192; Circle 187; Circle 186]);
   (Circle 186, [Circle 195; Circle 194; Circle 193; Circle 192; Circle 187]);
   (Circle 187, [Circle 195; Circle 194; Circle 193; Circle 192]);
   (Circle 188, [Circle 191; Circle 190]);
   (Circle 192, [Circle 195; Circle 194; Circle 193]);
   (Circle 193, [Circle 195; Circle 194]);
   (Circle 194, [Circle 195])]