(* 
   Christian Kaas
   Letters from Whitechapel - A Game Solver
   Spring 2018
*)

(* This module provides a model for a Whitechapel-like board game, implemented
   as an adjacency hashtable *)
   
type node_type = 
  | Circle of int
  | Square of (direction * node_type)
and direction =
  | NorthOf
  | SouthOf
  | EastOf
  | WestOf
  | SouthWestOf
  | SouthSouthOf

type move_type =
  | Move
  | Alleyway
  | Step

let steps =
 [(Circle 1, [Square (EastOf, Circle 1); Square (WestOf, Circle 1)]);
  (Square (EastOf, Circle 1), [Square (WestOf, Circle 2); Circle 26]);
  (Square (WestOf, Circle 1), [Square (NorthOf, Circle 6); Circle 7]);
  (Circle 2, [Square (EastOf, Circle 2); Square (WestOf, Circle 2)]);
  (Square (EastOf, Circle 2), [Circle 3; Square (NorthOf, Circle 9)]);
  (Square (WestOf, Circle 2), [Square (WestOf, Circle 9)]);
  (Circle 3, [Square (EastOf, Circle 3)]);
  (Square (EastOf, Circle 3), [Circle 4; Circle 5]);
  (Circle 4, [Square (SouthOf, Circle 4)]);
  (Square (SouthOf, Circle 4), [Square (EastOf, Circle 10); Circle 11; Circle 12]);
  (Circle 5, [Square (EastOf, Circle 5)]);
  (Square (EastOf, Circle 5), [Square (EastOf, Circle 12); Square (NorthOf, Circle 15)]);
  (Circle 6, [Square (NorthOf, Circle 6); Square (SouthOf, Circle 6)]);
  (Square (NorthOf, Circle 6), [Square (NorthOf, Circle 24)]);
  (Square (SouthOf, Circle 6), [Square (SouthOf, Circle 7); Circle 24; Circle 25]);
  (Circle 7, [Square (SouthOf, Circle 7)]);
  (Square (SouthOf, Circle 7), [Square (SouthOf, Circle 25); Circle 26]);
  (Circle 8, [Square (EastOf, Circle 8); Square (WestOf, Circle 8)]);
  (Square (EastOf, Circle 8), [Circle 9; Circle 10]);
  (Square (WestOf, Circle 8), [Square (WestOf, Circle 9); Circle 26; Circle 28]);
  (Circle 9, [Square (NorthOf, Circle 9); Square (WestOf, Circle 9)]);
  (Square (NorthOf, Circle 9), [Circle 11]);
  (Circle 10, [Square (EastOf, Circle 10)]);
  (Square (EastOf, Circle 10), [Circle 30]);
  (Circle 12, [Square (EastOf, Circle 12)]);
  (Square (EastOf, Circle 12), [Circle 13]);
  (Circle 13, [Square (SouthOf, Circle 13)]);
  (Square (SouthOf, Circle 13), [Circle 14; Square (EastOf, Circle 30)]);
  (Circle 14, [Square (SouthOf, Circle 14)]);
  (Square (SouthOf, Circle 14), [Square (EastOf, Circle 31); Circle 33]);
  (Circle 15, [Square (NorthOf, Circle 15); Square (SouthOf, Circle 15)]);
  (Square (NorthOf, Circle 15), [Square (NorthOf, Circle 16)]);
  (Square (SouthOf, Circle 15), [Square (SouthOf, Circle 16); Circle 33]);
  (Circle 16, [Square (NorthOf, Circle 16); Square (SouthOf, Circle 16)]);
  (Square (NorthOf, Circle 16), [Circle 17]);
  (Square (SouthOf, Circle 16), [Circle 35; Circle 36]);
  (Circle 17, [Square (EastOf, Circle 17); Square (SouthOf, Circle 17)]);
  (Square (EastOf, Circle 17), [Square (SouthOf, Circle 17); Circle 18]);
  (Square (SouthOf, Circle 17), [Circle 36; Circle 38]);
  (Circle 18, [Square (EastOf, Circle 18); Square (SouthOf, Circle 18)]);
  (Square (EastOf, Circle 18), [Square (EastOf, Circle 19)]);
  (Square (SouthOf, Circle 18), [Circle 19; Circle 39]);
  (Circle 19, [Square (EastOf, Circle 19)]);
  (Square (EastOf, Circle 19), [Circle 20]);
  (Circle 20, [Square (SouthOf, Circle 20)]);
  (Square (SouthOf, Circle 20), [Circle 40; Square (NorthOf, Circle 41)]);
  (Circle 21, [Square (EastOf, Circle 21); Square (WestOf, Circle 21)]);
  (Square (EastOf, Circle 21), [Square (NorthOf, Circle 23)]);
  (Square (WestOf, Circle 21), [Square (NorthOf, Circle 41); Square (NorthOf, Circle 42)]);
  (Circle 22, [Square (EastOf, Circle 22); Square (WestOf, Circle 22)]);
  (Square (EastOf, Circle 22), [Circle 23; Square (NorthOf, Circle 77)]);
  (Square (WestOf, Circle 22), [Circle 42]);
  (Circle 23, [Square (NorthOf, Circle 23)]);
  (Circle 24, [Square (NorthOf, Circle 24); Square (SouthOf, Circle 24)]);
  (Square (SouthOf, Circle 24), [Circle 25; Square (WestOf, Circle 44)]);
  (Circle 25, [Square (SouthOf, Circle 25)]);
  (Square (SouthOf, Circle 25), [Circle 44]);
  (Circle 26, [Square (SouthOf, Circle 26); Circle 28]);
  (Square (SouthOf, Circle 26), [Circle 27; Circle 44; Square (EastOf, Circle 44)]);
  (Circle 27, [Square (EastOf, Circle 27)]);
  (Square (EastOf, Circle 27), [Circle 28; Square (EastOf, Circle 46)]);
  (Circle 29, [Square (EastOf, Circle 29); Square (WestOf, Circle 29)]);
  (Square (EastOf, Circle 29), [Circle 30; Square (EastOf, Circle 49)]);
  (Square (WestOf, Circle 29), [Square (EastOf, Circle 46)]);
  (Circle 30, [Square (EastOf, Circle 30)]);
  (Circle 31, [Square (EastOf, Circle 31); Square (SouthOf, Circle 31); Square (WestOf, Circle 31)]);
  (Square (EastOf, Circle 31), [Circle 32; Square (EastOf, Circle 52)]);
  (Square (SouthOf, Circle 31), [Circle 52]);
  (Square (WestOf, Circle 31), [Circle 50; Square (EastOf, Circle 51)]);
  (Circle 34, [Square (SouthOf, Circle 34)]);
  (Square (SouthOf, Circle 34), [Square (SouthOf, Circle 35); Square (EastOf, Circle 53); Circle 55]);
  (Circle 35, [Square (SouthOf, Circle 35)]);
  (Square (SouthOf, Circle 35), [Circle 37]);
  (Circle 37, [Square (EastOf, Circle 37)]);
  (Square (EastOf, Circle 37), [Circle 38; Circle 39]);
  (Circle 39, [Square (SouthOf, Circle 39)]);
  (Square (SouthOf, Circle 39), [Circle 56]);
  (Circle 40, [Square (SouthOf, Circle 40)]);
  (Square (SouthOf, Circle 40), [Square (SouthOf, Circle 41); Circle 57; Square (WestOf, Circle 58)]);
  (Circle 41, [Square (NorthOf, Circle 41); Square (SouthOf, Circle 41)]);
  (Square (SouthOf, Circle 41), [Circle 42; Circle 58]);
  (Circle 42, [Square (NorthOf, Circle 42)]);
  (Circle 43, [Square (NorthOf, Circle 43); Square (EastOf, Circle 43)]);
  (Square (NorthOf, Circle 43), [Square (WestOf, Circle 44)]);
  (Square (EastOf, Circle 43), [Square (NorthOf, Circle 59)]);
  (Circle 44, [Square (EastOf, Circle 44); Square (WestOf, Circle 44)]);
  (Square (EastOf, Circle 44), [Circle 46; Square (NorthOf, Circle 79)]);
  (Square (WestOf, Circle 44), [Square (NorthOf, Circle 59)]);
  (Circle 45, [Circle 47; Square (EastOf, Circle 45); Square (SouthOf, Circle 45)]);
  (Square (EastOf, Circle 45), [Square (EastOf, Circle 46); Circle 47]);
  (Square (SouthOf, Circle 45), [Circle 47; Square (SouthOf, Circle 47)]);
  (Circle 46, [Square (EastOf, Circle 46)]);
  (Square (EastOf, Circle 46), [Circle 48]);
  (Circle 47, [Square (SouthOf, Circle 47)]);
  (Square (SouthOf, Circle 47), [Circle 61]);
  (Circle 48, [Square (EastOf, Circle 48); Square (SouthOf, Circle 48)]);
  (Square (EastOf, Circle 48), [Circle 49; Square (NorthOf, Circle 63)]);
  (Square (SouthOf, Circle 48), [Circle 62; Square (NorthOf, Circle 63)]);
  (Circle 49, [Square (EastOf, Circle 49)]);
  (Square (EastOf, Circle 49), [Circle 50; Square (EastOf, Circle 64)]);
  (Circle 51, [Square (EastOf, Circle 51); Square (SouthOf, Circle 51)]);
  (Square (EastOf, Circle 51), [Circle 52]);
  (Square (SouthOf, Circle 51), [Square (EastOf, Circle 65); Circle 67]);
  (Circle 52, [Square (EastOf, Circle 52); Square (SouthOf, Circle 52)]);
  (Square (EastOf, Circle 52), [Circle 54]);
  (Square (SouthOf, Circle 52), [Circle 53; Circle 67]);
  (Circle 53, [Square (EastOf, Circle 53); Square (SouthOf, Circle 53)]);
  (Square (EastOf, Circle 53), [Square (SouthOf, Circle 53); Circle 54]);
  (Square (SouthOf, Circle 53), [Circle 68]);
  (Circle 55, [Square (SouthOf, Circle 55)]);
  (Square (SouthOf, Circle 55), [Square (WestOf, Circle 56); Square (SouthOf, Circle 68)]);
  (Circle 56, [Square (EastOf, Circle 56); Square (WestOf, Circle 56)]);
  (Square (EastOf, Circle 56), [Circle 57]);
  (Circle 58, [Square (SouthOf, Circle 58); Square (WestOf, Circle 58)]);
  (Square (SouthOf, Circle 58), [Circle 73; Square (EastOf, Circle 74)]);
  (Square (WestOf, Circle 58), [Circle 73]);
  (Circle 59, [Square (NorthOf, Circle 59); Square (SouthOf, Circle 59)]);
  (Square (SouthOf, Circle 59), [Circle 60; Square (EastOf, Circle 95)]);
  (Circle 60, [Square (EastOf, Circle 60)]);
  (Square (EastOf, Circle 60), [Square (NorthOf, Circle 78)]);
  (Circle 62, [Square (SouthOf, Circle 62)]);
  (Square (SouthOf, Circle 62), [Circle 80; Circle 82; Square (NorthOf, Circle 98)]);
  (Circle 63, [Square (NorthOf, Circle 63); Square (SouthOf, Circle 63)]);
  (Square (NorthOf, Circle 63), [Circle 64]);
  (Square (SouthOf, Circle 63), [Square (SouthOf, Circle 65); Circle 82]);
  (Circle 64, [Square (EastOf, Circle 64)]);
  (Square (EastOf, Circle 64), [Circle 66]);
  (Circle 65, [Square (EastOf, Circle 65); Square (SouthOf, Circle 65)]);
  (Square (EastOf, Circle 65), [Circle 66; Square (NorthOf, Circle 84)]);
  (Square (SouthOf, Circle 65), [Circle 83]);
  (Circle 68, [Square (SouthOf, Circle 68)]);
  (Square (SouthOf, Circle 68), [Circle 86]);
  (Circle 69, [Square (EastOf, Circle 69); Square (SouthOf, Circle 69); Square (WestOf, Circle 69)]);
  (Square (EastOf, Circle 69), [Circle 70; Circle 103]);
  (Square (SouthOf, Circle 69), [Circle 127]);
  (Square (WestOf, Circle 69), [Circle 102]);
  (Circle 70, [Square (EastOf, Circle 70)]);
  (Square (EastOf, Circle 70), [Circle 71; Circle 87]);
  (Circle 71, [Square (EastOf, Circle 71); Square (SouthOf, Circle 71)]);
  (Square (EastOf, Circle 71), [Circle 72; Circle 88]);
  (Square (SouthOf, Circle 71), [Circle 104]);
  (Circle 72, [Square (EastOf, Circle 72)]);
  (Square (EastOf, Circle 72), [Circle 73; Circle 74; Square (NorthOf, Circle 89)]);
  (Circle 74, [Square (EastOf, Circle 74)]);
  (Square (EastOf, Circle 74), [Circle 75; Circle 76]);
  (Circle 75, [Square (SouthOf, Circle 75)]);
  (Square (SouthOf, Circle 75), [Circle 90; Square (EastOf, Circle 91); Square (NorthOf, Circle 94)]);
  (Circle 76, [Square (EastOf, Circle 76)]);
  (Square (EastOf, Circle 76), [Circle 77; Square (NorthOf, Circle 94)]);
  (Circle 77, [Square (NorthOf, Circle 77)]);
  (Circle 78, [Square (NorthOf, Circle 78); Square (EastOf, Circle 78); Square (WestOf, Circle 78)]);
  (Square (NorthOf, Circle 78), [Circle 79]);
  (Square (EastOf, Circle 78), [Circle 79; Circle 80; Circle 97]);
  (Square (WestOf, Circle 78), [Square (EastOf, Circle 95); Circle 96]);
  (Circle 79, [Square (NorthOf, Circle 79)]);
  (Circle 80, [Square (SouthOf, Circle 80)]);
  (Square (SouthOf, Circle 80), [Circle 81]);
  (Circle 81, [Square (SouthOf, Circle 81)]);
  (Square (SouthOf, Circle 81), [Circle 118]);
  (Circle 83, [Square (SouthOf, Circle 83)]);
  (Square (SouthOf, Circle 83), [Square (WestOf, Circle 99)]);
  (Circle 84, [Square (NorthOf, Circle 84); Square (SouthOf, Circle 84)]);
  (Square (SouthOf, Circle 84), [Square (EastOf, Circle 99)]);
  (Circle 85, [Square (NorthOf, Circle 85); Square (SouthOf, Circle 85)]);
  (Square (NorthOf, Circle 85), [Circle 86; Square (NorthOf, Circle 100)]);
  (Square (SouthOf, Circle 85), [Circle 101; Square (NorthOf, Circle 124)]);
  (Circle 87, [Square (SouthOf, Circle 87)]);
  (Square (SouthOf, Circle 87), [Square (WestOf, Circle 104)]);
  (Circle 88, [Square (SouthOf, Circle 88)]);
  (Square (SouthOf, Circle 88), [Square (EastOf, Circle 104)]);
  (Circle 89, [Square (NorthOf, Circle 89); Square (SouthOf, Circle 89)]);
  (Square (NorthOf, Circle 89), [Circle 90]);
  (Square (SouthOf, Circle 89), [Circle 91; Square (EastOf, Circle 105)]);
  (Circle 91, [Square (EastOf, Circle 91)]);
  (Square (EastOf, Circle 91), [Circle 92; Circle 93]);
  (Circle 92, [Square (SouthOf, Circle 92)]);
  (Square (SouthOf, Circle 92), [Circle 107; Square (EastOf, Circle 108); Circle 109]);
  (Circle 93, [Square (EastOf, Circle 93)]);
  (Square (EastOf, Circle 93), [Circle 94; Square (EastOf, Circle 109)]);
  (Circle 94, [Square (NorthOf, Circle 94)]);
  (Circle 95, [Square (EastOf, Circle 95); Square (WestOf, Circle 95)]);
  (Square (WestOf, Circle 95), [Square (WestOf, Circle 96); Square (EastOf, Circle 112)]);
  (Circle 96, [Square (SouthOf, Circle 96); Square (WestOf, Circle 96)]);
  (Square (SouthOf, Circle 96), [Circle 97; Square (EastOf, Circle 115)]);
  (Square (WestOf, Circle 96), [Circle 114]);
  (Circle 97, [Square (SouthOf, Circle 97)]);
  (Square (SouthOf, Circle 97), [Circle 117]);
  (Circle 98, [Square (NorthOf, Circle 98); Square (SouthOf, Circle 98)]);
  (Square (SouthOf, Circle 98), [Square (EastOf, Circle 118)]);
  (Circle 99, [Square (EastOf, Circle 99); Square (WestOf, Circle 99)]);
  (Square (EastOf, Circle 99), [Square (NorthOf, Circle 100)]);
  (Square (WestOf, Circle 99), [Circle 100; Circle 120]);
  (Circle 100, [Square (NorthOf, Circle 100); Square (SouthOf, Circle 100)]);
  (Square (SouthOf, Circle 100), [Circle 122; Square (EastOf, Circle 123); Square (WestOf, Circle 124)]);
  (Circle 101, [Square (SouthOf, Circle 101)]);
  (Square (SouthOf, Circle 101), [Square (SouthOf, Circle 102); Circle 126; Square (WestOf, Circle 143)]);
  (Circle 102, [Square (SouthOf, Circle 102)]);
  (Square (SouthOf, Circle 102), [Circle 127]);
  (Circle 103, [Square (SouthOf, Circle 103)]);
  (Square (SouthOf, Circle 103), [Circle 127; Circle 128]);
  (Circle 104, [Square (EastOf, Circle 104); Square (SouthOf, Circle 104); Square (WestOf, Circle 104)]);
  (Square (EastOf, Circle 104), [Circle 105; Circle 130]);
  (Square (SouthOf, Circle 104), [Circle 145]);
  (Square (WestOf, Circle 104), [Circle 129]);
  (Circle 105, [Square (EastOf, Circle 105)]);
  (Square (EastOf, Circle 105), [Circle 106; Circle 107]);
  (Circle 106, [Square (SouthOf, Circle 106)]);
  (Square (SouthOf, Circle 106), [Circle 108; Square (NorthOf, Circle 131)]);
  (Circle 108, [Square (EastOf, Circle 108)]);
  (Square (EastOf, Circle 108), [Circle 110; Circle 132]);
  (Circle 109, [Square (EastOf, Circle 109)]);
  (Square (EastOf, Circle 109), [Square (EastOf, Circle 110)]);
  (Circle 110, [Square (EastOf, Circle 110)]);
  (Square (EastOf, Circle 110), [Circle 111]);
  (Circle 111, [Square (SouthOf, Circle 111)]);
  (Square (SouthOf, Circle 111), [Circle 134; Circle 147]);
  (Circle 112, [Square (EastOf, Circle 112); Square (SouthOf, Circle 112)]);
  (Square (EastOf, Circle 112), [Circle 113]);
  (Square (SouthOf, Circle 112), [Circle 135; Square (WestOf, Circle 148)]);
  (Circle 113, [Square (SouthOf, Circle 113)]);
  (Square (SouthOf, Circle 113), [Circle 114; Square (WestOf, Circle 137); Square (SouthOf, Circle 135)]);
  (Circle 114, [Square (SouthOf, Circle 114)]);
  (Square (SouthOf, Circle 114), [Circle 115; Circle 137]);
  (Circle 115, [Square (EastOf, Circle 115)]);
  (Square (EastOf, Circle 115), [Circle 116]);
  (Circle 116, [Square (SouthOf, Circle 116)]);
  (Square (SouthOf, Circle 116), [Square (EastOf, Circle 139)]);
  (Circle 117, [Square (SouthOf, Circle 117)]);
  (Square (SouthOf, Circle 117), [Square (WestOf, Circle 118)]);
  (Circle 118, [Square (EastOf, Circle 118); Square (WestOf, Circle 118)]);
  (Square (EastOf, Circle 118), [Circle 119; Circle 120; Square (NorthOf, Circle 121)]);
  (Square (WestOf, Circle 118), [Square (EastOf, Circle 139); Square (NorthOf, Circle 151)]);
  (Circle 119, [Square (SouthOf, Circle 119)]);
  (Square (SouthOf, Circle 119), [Circle 121; Square (EastOf, Circle 151)]);
  (Circle 121, [Square (NorthOf, Circle 121); Square (SouthOf, Circle 121)]);
  (Square (NorthOf, Circle 121), [Square (WestOf, Circle 122)]);
  (Square (SouthOf, Circle 121), [Square (EastOf, Circle 151)]);
  (Circle 122, [Square (WestOf, Circle 122)]);
  (Square (WestOf, Circle 122), [Circle 123]);
  (Circle 123, [Square (EastOf, Circle 123)]);
  (Square (EastOf, Circle 123), [Square (WestOf, Circle 124); Circle 125; Square (EastOf, Circle 140); Square (EastOf, Circle 141)]);
  (Circle 124, [Square (NorthOf, Circle 124); Square (WestOf, Circle 124)]);
  (Circle 125, [Square (EastOf, Circle 125)]);
  (Square (EastOf, Circle 125), [Circle 126; Square (WestOf, Circle 143)]);
  (Circle 128, [Square (SouthOf, Circle 128)]);
  (Square (SouthOf, Circle 128), [Circle 143; Circle 144]);
  (Circle 129, [Square (SouthOf, Circle 129)]);
  (Square (SouthOf, Circle 129), [Circle 144; Circle 145]);
  (Circle 130, [Square (SouthOf, Circle 130)]);
  (Square (SouthOf, Circle 130), [Square (SouthWestOf, Circle 131); Circle 145; Square (NorthOf, Circle 161)]);
  (Circle 131, [Square (NorthOf, Circle 131); Square (SouthOf, Circle 131)]);
  (Square (NorthOf, Circle 131), [Square (SouthOf, Circle 132)]);
  (Square (SouthOf, Circle 131), [Square (SouthWestOf, Circle 131); Circle 146]);
  (Square (SouthWestOf, Circle 131), [Square (EastOf, Circle 161)]);
  (Circle 132, [Square (SouthOf, Circle 132)]);
  (Square (SouthOf, Circle 132), [Circle 133; Circle 134]);
  (Circle 133, [Square (SouthOf, Circle 133)]);
  (Square (SouthOf, Circle 133), [Circle 146; Circle 147]);
  (Circle 135, [Square (SouthOf, Circle 135)]);
  (Square (SouthOf, Circle 135), [Circle 136; Circle 148]);
  (Circle 136, [Square (SouthOf, Circle 136)]);
  (Square (SouthOf, Circle 136), [Circle 149; Square (EastOf, Circle 163)]);
  (Circle 137, [Square (WestOf, Circle 137)]);
  (Square (WestOf, Circle 137), [Circle 138]);
  (Circle 138, [Square (SouthOf, Circle 138)]);
  (Square (SouthOf, Circle 138), [Square (WestOf, Circle 139); Square (EastOf, Circle 163); Circle 164]);
  (Circle 139, [Square (EastOf, Circle 139); Square (WestOf, Circle 139)]);
  (Square (EastOf, Circle 139), [Circle 150; Square (NorthOf, Circle 151)]);
  (Square (WestOf, Circle 139), [Circle 164]);
  (Circle 140, [Square (EastOf, Circle 140); Square (WestOf, Circle 140)]);
  (Square (EastOf, Circle 140), [Square (WestOf, Circle 141)]);
  (Square (WestOf, Circle 140), [Square (EastOf, Circle 152); Circle 154]);
  (Circle 141, [Square (EastOf, Circle 141); Square (WestOf, Circle 141)]);
  (Square (EastOf, Circle 141), [Circle 155]);
  (Square (WestOf, Circle 141), [Circle 170]);
  (Circle 142, [Square (EastOf, Circle 142); Square (WestOf, Circle 142)]);
  (Square (EastOf, Circle 142), [Circle 143; Square (NorthOf, Circle 158)]);
  (Square (WestOf, Circle 142), [Square (WestOf, Circle 143); Circle 156]);
  (Circle 143, [Square (WestOf, Circle 143)]);
  (Circle 145, [Square (SouthOf, Circle 145)]);
  (Square (SouthOf, Circle 145), [Circle 160]);
  (Circle 148, [Square (SouthOf, Circle 148); Square (WestOf, Circle 148)]);
  (Square (SouthOf, Circle 148), [Circle 149; Square (SouthOf, Circle 162)]);
  (Square (WestOf, Circle 148), [Circle 162]);
  (Circle 150, [Square (SouthOf, Circle 150)]);
  (Square (SouthOf, Circle 150), [Square (WestOf, Circle 151); Square (WestOf, Circle 166)]);
  (Circle 151, [Square (NorthOf, Circle 151); Square (EastOf, Circle 151); Square (WestOf, Circle 151)]);
  (Square (EastOf, Circle 151), [Circle 153]);
  (Circle 152, [Square (EastOf, Circle 152); Square (WestOf, Circle 152)]);
  (Square (EastOf, Circle 152), [Circle 153; Square (WestOf, Circle 168)]);
  (Square (WestOf, Circle 152), [Square (EastOf, Circle 165); Circle 166]);
  (Circle 154, [Square (SouthOf, Circle 154)]);
  (Square (SouthOf, Circle 154), [Circle 168; Circle 169; Circle 170]);
  (Circle 155, [Square (SouthOf, Circle 155)]);
  (Square (SouthOf, Circle 155), [Square (SouthOf, Circle 170); Circle 182]);
  (Circle 156, [Square (SouthOf, Circle 156)]);
  (Square (SouthOf, Circle 156), [Square (SouthSouthOf, Circle 156); Circle 157]);
  (Square (SouthSouthOf, Circle 156), [Square (WestOf, Circle 171)]);
  (Circle 157, [Square (EastOf, Circle 157)]);
  (Square (EastOf, Circle 157), [Circle 158; Square (WestOf, Circle 159); Square (EastOf, Circle 171)]);
  (Circle 158, [Square (NorthOf, Circle 158)]);
  (Circle 159, [Square (NorthOf, Circle 159); Square (EastOf, Circle 159); Square (SouthOf, Circle 159); Square (WestOf, Circle 159)]);
  (Square (EastOf, Circle 159), [Circle 160; Circle 173]);
  (Square (SouthOf, Circle 159), [Square (SouthOf, Circle 173); Square (EastOf, Circle 185)]);
  (Square (WestOf, Circle 159), [Circle 172]);
  (Circle 160, [Square (SouthOf, Circle 160)]);
  (Square (SouthOf, Circle 160), [Square (SouthOf, Circle 161)]);
  (Circle 161, [Square (NorthOf, Circle 161); Square (EastOf, Circle 161); Square (SouthOf, Circle 161)]);
  (Circle 162, [Square (SouthOf, Circle 162)]);
  (Square (SouthOf, Circle 162), [Circle 163]);
  (Circle 163, [Square (EastOf, Circle 163)]);
  (Square (EastOf, Circle 163), [Circle 174]);
  (Circle 164, [Square (SouthOf, Circle 164)]);
  (Square (SouthOf, Circle 164), [Circle 174; Square (NorthOf, Circle 175); Square (WestOf, Circle 188)]);
  (Circle 165, [Square (EastOf, Circle 165); Square (SouthOf, Circle 165)]);
  (Square (EastOf, Circle 165), [Circle 167; Square (EastOf, Circle 177)]);
  (Square (SouthOf, Circle 165), [Circle 177; Square (WestOf, Circle 178)]);
  (Circle 166, [Square (WestOf, Circle 166)]);
  (Square (WestOf, Circle 166), [Circle 176]);
  (Circle 167, [Square (SouthOf, Circle 167)]);
  (Square (SouthOf, Circle 167), [Circle 178; Circle 179]);
  (Circle 168, [Square (WestOf, Circle 168)]);
  (Square (WestOf, Circle 168), [Circle 180]);
  (Circle 169, [Square (SouthOf, Circle 169)]);
  (Square (SouthOf, Circle 169), [Square (SouthOf, Circle 180); Circle 181]);
  (Circle 170, [Square (SouthOf, Circle 170)]);
  (Square (SouthOf, Circle 170), [Circle 181]);
  (Circle 171, [Square (EastOf, Circle 171); Square (WestOf, Circle 171)]);
  (Square (EastOf, Circle 171), [Square (EastOf, Circle 183); Circle 185]);
  (Square (WestOf, Circle 171), [Circle 183]);
  (Circle 172, [Square (SouthOf, Circle 172)]);
  (Square (SouthOf, Circle 172), [Circle 185; Square (EastOf, Circle 185)]);
  (Circle 173, [Square (SouthOf, Circle 173)]);
  (Square (SouthOf, Circle 173), [Square (EastOf, Circle 185); Circle 195]);
  (Circle 175, [Square (NorthOf, Circle 175); Square (SouthOf, Circle 175)]);
  (Square (SouthOf, Circle 175), [Square (EastOf, Circle 188)]);
  (Circle 176, [Square (SouthOf, Circle 176)]);
  (Square (SouthOf, Circle 176), [Square (EastOf, Circle 188); Square (WestOf, Circle 190)]);
  (Circle 177, [Square (EastOf, Circle 177)]);
  (Square (EastOf, Circle 177), [Circle 178]);
  (Circle 178, [Square (SouthOf, Circle 178); Square (WestOf, Circle 178)]);
  (Square (SouthOf, Circle 178), [Circle 179; Circle 189]);
  (Square (WestOf, Circle 178), [Circle 189]);
  (Circle 180, [Square (SouthOf, Circle 180)]);
  (Square (SouthOf, Circle 180), [Circle 191]);
  (Circle 182, [Square (EastOf, Circle 182)]);
  (Square (EastOf, Circle 182), [Circle 183; Square (SouthOf, Circle 183); Circle 192]);
  (Circle 183, [Square (EastOf, Circle 183); Square (SouthOf, Circle 183)]);
  (Square (EastOf, Circle 183), [Circle 185]);
  (Square (SouthOf, Circle 183), [Circle 184; Square (WestOf, Circle 185)]);
  (Circle 184, [Square (SouthOf, Circle 184)]);
  (Square (SouthOf, Circle 184), [Circle 192; Square (SouthOf, Circle 193)]);
  (Circle 185, [Square (EastOf, Circle 185); Square (WestOf, Circle 185)]);
  (Square (EastOf, Circle 185), [Circle 187]);
  (Square (WestOf, Circle 185), [Square (WestOf, Circle 186)]);
  (Circle 186, [Square (EastOf, Circle 186); Square (WestOf, Circle 186)]);
  (Square (EastOf, Circle 186), [Circle 187; Square (EastOf, Circle 193)]);
  (Square (WestOf, Circle 186), [Circle 193]);
  (Circle 188, [Square (EastOf, Circle 188); Square (WestOf, Circle 188)]);
  (Circle 190, [Square (EastOf, Circle 190); Square (WestOf, Circle 190)]);
  (Square (EastOf, Circle 190), [Circle 191]);
  (Circle 193, [Square (EastOf, Circle 193); Square (SouthOf, Circle 193)]);
  (Square (EastOf, Circle 193), [Square (EastOf, Circle 194)]);
  (Square (SouthOf, Circle 193), [Circle 194]);
  (Circle 194, [Square (EastOf, Circle 194)]);
  (Square (EastOf, Circle 194), [Circle 195])]

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
