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

(* Board movement data below are adapted from Dienes' excellent
   "Logical Map Structure CSV" for Whitechapel
   https://boardgamegeek.com/filepage/162384/logical-map-structure-csv *)

open Geometry

let alleyways =
 [(Circle 1, [Circle 7; Circle 26]);
  (Circle 2, [Circle 9]);
  (Circle 3, [Circle 4; Circle 11]);
  (Circle 4, [Circle 5; Circle 11; Circle 12]);
  (Circle 5, [Circle 12]);
  (Circle 6, [Circle 7; Circle 24]);
  (Circle 7, [Circle 26]);
  (Circle 8, [Circle 9; Circle 10; Circle 28; Circle 29; Circle 30]);
  (Circle 9, [Circle 10; Circle 11]);
  (Circle 10, [Circle 11; Circle 28; Circle 29; Circle 30]);
  (Circle 12, [Circle 13; Circle 30]);
  (Circle 13, [Circle 14; Circle 15; Circle 30; Circle 33]);
  (Circle 14, [Circle 15; Circle 32; Circle 33]);
  (Circle 15, [Circle 16; Circle 33]);
  (Circle 16, [Circle 17; Circle 36]);
  (Circle 17, [Circle 36]);
  (Circle 18, [Circle 19; Circle 38; Circle 39]);
  (Circle 19, [Circle 20; Circle 39; Circle 40; Circle 56; Circle 57]);
  (Circle 20, [Circle 39; Circle 40; Circle 56; Circle 57]);
  (Circle 21, [Circle 22; Circle 23; Circle 42]);
  (Circle 22, [Circle 23; Circle 42; Circle 58; Circle 76; Circle 77]);
  (Circle 23, [Circle 42]);
  (Circle 24, [Circle 25]);
  (Circle 25, [Circle 44]);
  (Circle 26, [Circle 27; Circle 28; Circle 44]);
  (Circle 27, [Circle 28; Circle 46]);
  (Circle 28, [Circle 29; Circle 30]);
  (Circle 29, [Circle 30; Circle 48; Circle 49]);
  (Circle 30, [Circle 31; Circle 32; Circle 50]);
  (Circle 31, [Circle 32; Circle 50; Circle 52]);
  (Circle 32, [Circle 50]);
  (Circle 33, [Circle 34; Circle 54]);
  (Circle 34, [Circle 35; Circle 54]);
  (Circle 35, [Circle 36; Circle 37; Circle 38]);
  (Circle 36, [Circle 37; Circle 38]);
  (Circle 37, [Circle 38; Circle 39; Circle 55; Circle 56]);
  (Circle 38, [Circle 39]);
  (Circle 39, [Circle 40; Circle 55; Circle 56; Circle 57]);
  (Circle 40, [Circle 41; Circle 56; Circle 57]);
  (Circle 41, [Circle 42]);
  (Circle 42, [Circle 58; Circle 76; Circle 77]);
  (Circle 44, [Circle 59; Circle 60; Circle 79]);
  (Circle 45, [Circle 46; Circle 47; Circle 48; Circle 62; Circle 79; Circle 80]);
  (Circle 46, [Circle 48; Circle 62; Circle 79; Circle 80]);
  (Circle 47, [Circle 48; Circle 62; Circle 79; Circle 80]);
  (Circle 48, [Circle 49; Circle 62; Circle 79; Circle 80]);
  (Circle 49, [Circle 64]);
  (Circle 50, [Circle 51; Circle 66]);
  (Circle 51, [Circle 52; Circle 66; Circle 67]);
  (Circle 52, [Circle 53; Circle 54; Circle 67]);
  (Circle 53, [Circle 54; Circle 67; Circle 68; Circle 86]);
  (Circle 55, [Circle 56; Circle 68]);
  (Circle 56, [Circle 57; Circle 69; Circle 70; Circle 71; Circle 72; Circle 73]);
  (Circle 57, [Circle 69; Circle 70; Circle 71; Circle 72; Circle 73]);
  (Circle 58, [Circle 73; Circle 76; Circle 77]);
  (Circle 59, [Circle 60; Circle 79]);
  (Circle 60, [Circle 78; Circle 79]);
  (Circle 61, [Circle 62; Circle 79; Circle 80]);
  (Circle 62, [Circle 63; Circle 79; Circle 80; Circle 82]);
  (Circle 63, [Circle 64; Circle 65; Circle 66; Circle 82]);
  (Circle 64, [Circle 65; Circle 66]);
  (Circle 65, [Circle 66; Circle 83; Circle 84; Circle 99]);
  (Circle 67, [Circle 68; Circle 84; Circle 86]);
  (Circle 68, [Circle 84; Circle 86]);
  (Circle 69, [Circle 70; Circle 71; Circle 72; Circle 73; Circle 102; Circle 103; Circle 127]);
  (Circle 70, [Circle 71; Circle 72; Circle 73; Circle 87; Circle 103; Circle 128; Circle 129; Circle 144]);
  (Circle 71, [Circle 72; Circle 73; Circle 87; Circle 88; Circle 104]);
  (Circle 72, [Circle 73; Circle 88; Circle 89; Circle 105]);
  (Circle 73, [Circle 74]);
  (Circle 74, [Circle 75; Circle 90]);
  (Circle 75, [Circle 76; Circle 90]);
  (Circle 76, [Circle 77]);
  (Circle 78, [Circle 79; Circle 96; Circle 97]);
  (Circle 79, [Circle 80]);
  (Circle 80, [Circle 81; Circle 97; Circle 98; Circle 117; Circle 118]);
  (Circle 81, [Circle 97; Circle 98; Circle 117; Circle 118]);
  (Circle 82, [Circle 83; Circle 98; Circle 120]);
  (Circle 83, [Circle 84; Circle 98; Circle 99; Circle 120]);
  (Circle 84, [Circle 86; Circle 99]);
  (Circle 85, [Circle 86; Circle 100; Circle 101; Circle 102; Circle 124]);
  (Circle 86, [Circle 101; Circle 102]);
  (Circle 87, [Circle 103; Circle 104; Circle 128; Circle 129; Circle 144]);
  (Circle 88, [Circle 89; Circle 104; Circle 105]);
  (Circle 89, [Circle 90; Circle 91; Circle 105]);
  (Circle 90, [Circle 91]);
  (Circle 91, [Circle 92; Circle 107]);
  (Circle 92, [Circle 93; Circle 107; Circle 109]);
  (Circle 93, [Circle 94; Circle 109]);
  (Circle 95, [Circle 96]);
  (Circle 96, [Circle 97; Circle 114; Circle 115]);
  (Circle 97, [Circle 116; Circle 117; Circle 118]);
  (Circle 98, [Circle 118; Circle 120]);
  (Circle 99, [Circle 100]);
  (Circle 100, [Circle 120; Circle 122; Circle 124]);
  (Circle 101, [Circle 102; Circle 126]);
  (Circle 102, [Circle 127]);
  (Circle 103, [Circle 127; Circle 128; Circle 129; Circle 144]);
  (Circle 104, [Circle 129; Circle 130; Circle 145]);
  (Circle 105, [Circle 106; Circle 130; Circle 131]);
  (Circle 106, [Circle 107; Circle 108; Circle 130; Circle 131]);
  (Circle 107, [Circle 108]);
  (Circle 108, [Circle 132]);
  (Circle 109, [Circle 110]);
  (Circle 110, [Circle 111; Circle 132; Circle 134]);
  (Circle 111, [Circle 132; Circle 134]);
  (Circle 112, [Circle 113; Circle 135]);
  (Circle 113, [Circle 114; Circle 135]);
  (Circle 114, [Circle 115; Circle 137]);
  (Circle 115, [Circle 116; Circle 137; Circle 138; Circle 139]);
  (Circle 116, [Circle 117; Circle 137; Circle 138; Circle 139]);
  (Circle 117, [Circle 118]);
  (Circle 118, [Circle 119]);
  (Circle 119, [Circle 121; Circle 151]);
  (Circle 120, [Circle 122]);
  (Circle 121, [Circle 123; Circle 140; Circle 153]);
  (Circle 122, [Circle 123]);
  (Circle 123, [Circle 140; Circle 153]);
  (Circle 124, [Circle 125; Circle 126]);
  (Circle 125, [Circle 126; Circle 155; Circle 156; Circle 182; Circle 183]);
  (Circle 127, [Circle 128; Circle 143]);
  (Circle 128, [Circle 129; Circle 143; Circle 144]);
  (Circle 129, [Circle 144; Circle 145]);
  (Circle 130, [Circle 131; Circle 145]);
  (Circle 131, [Circle 133; Circle 146]);
  (Circle 132, [Circle 134]);
  (Circle 133, [Circle 134; Circle 146; Circle 147]);
  (Circle 134, [Circle 147]);
  (Circle 135, [Circle 148]);
  (Circle 136, [Circle 138; Circle 148; Circle 149]);
  (Circle 137, [Circle 138; Circle 139]);
  (Circle 138, [Circle 139]);
  (Circle 139, [Circle 150; Circle 164; Circle 175; Circle 176]);
  (Circle 140, [Circle 153; Circle 154; Circle 170]);
  (Circle 141, [Circle 155; Circle 170]);
  (Circle 142, [Circle 143; Circle 156; Circle 157; Circle 158]);
  (Circle 143, [Circle 158; Circle 159]);
  (Circle 144, [Circle 159]);
  (Circle 145, [Circle 160; Circle 161]);
  (Circle 148, [Circle 149; Circle 162]);
  (Circle 149, [Circle 163]);
  (Circle 150, [Circle 151; Circle 164; Circle 175; Circle 176]);
  (Circle 151, [Circle 152; Circle 153; Circle 166]);
  (Circle 152, [Circle 153; Circle 166; Circle 167; Circle 179; Circle 180; Circle 191]);
  (Circle 153, [Circle 166]);
  (Circle 154, [Circle 168; Circle 170]);
  (Circle 155, [Circle 156; Circle 170; Circle 182; Circle 183]);
  (Circle 156, [Circle 157; Circle 158; Circle 182; Circle 183]);
  (Circle 157, [Circle 158; Circle 171]);
  (Circle 158, [Circle 159]);
  (Circle 159, [Circle 172; Circle 173]);
  (Circle 160, [Circle 161]);
  (Circle 164, [Circle 174; Circle 175; Circle 176]);
  (Circle 165, [Circle 166; Circle 176; Circle 177; Circle 189; Circle 190]);
  (Circle 166, [Circle 176; Circle 189; Circle 190]);
  (Circle 167, [Circle 178; Circle 179; Circle 180; Circle 191]);
  (Circle 168, [Circle 169; Circle 180]);
  (Circle 169, [Circle 170; Circle 180; Circle 181]);
  (Circle 170, [Circle 181]);
  (Circle 171, [Circle 183]);
  (Circle 172, [Circle 185]);
  (Circle 175, [Circle 176; Circle 188]);
  (Circle 176, [Circle 189; Circle 190]);
  (Circle 177, [Circle 178]);
  (Circle 178, [Circle 179; Circle 189]);
  (Circle 179, [Circle 180; Circle 191]);
  (Circle 180, [Circle 191]);
  (Circle 182, [Circle 183]);
  (Circle 183, [Circle 185]);
  (Circle 184, [Circle 192; Circle 193]);
  (Circle 185, [Circle 186; Circle 187]);
  (Circle 186, [Circle 187; Circle 193]);
  (Circle 187, [Circle 195]);
  (Circle 189, [Circle 190]);
  (Circle 193, [Circle 194])]
