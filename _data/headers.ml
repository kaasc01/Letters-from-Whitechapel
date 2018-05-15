let header = "\
(**************************************************************************)
(*                                                                        *)
(*  Letters from Whitechapel: A Model and Solver                          *)
(*  Copyright (C) 2018                                                    *)
(*  Christian Kaas                                                        *)
(*                                                                        *)
(*  Based on the Cobblepot Games' popular \"Letters from Whitechapel\"      *)
(*  boardgame by Gabriele Mari and Gianluca Santopietro                   *)
(*  http://www.sirchestercobblepot.com/en/letters-from-whitechapel/       *)
(*                                                                        *)
(**************************************************************************) "

let moves_comment = "\
(* As Dienes' CSV mostly describes the visual connections between different
   nodes on the board (Circle -> Square -> Circle/Square) and not abstract
   edges (regular move, or carriage move) that can be traversed by Jack, we
   need to handle this logic separately.

   This file summarizes all possible regular moves that can be made from
   each circle, as infered from the Dienes' step data.

   Note: Each edge is listed only only once - for example, once Circle 1
   -> Circle 2 is listed, Circle 2 -> Circle 1 is not listed. It follows
   that each circle only connects to circles of higher index. *) "


let carriages_comment = "\
(* As Dienes' CSV mostly describes the visual connections between different
   nodes on the board (Circle -> Square -> Circle/Square) and not abstract
   edges (regular move, or carriage move) that can be traversed by Jack, we
   need to handle this logic separately.

   This file summarizes all possible carriage moves that can be made from
   each circle, as infered from the Dienes' step data, and the infered
   regular move data.

   Note: Each edge is listed only only once - for example, once Circle 1
   -> Circle 2 is listed, Circle 2 -> Circle 1 is not listed. It follows
  that each circle only connects to circles of higher index. *) "
