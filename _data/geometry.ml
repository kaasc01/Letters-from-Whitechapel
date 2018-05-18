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

(* This file provides some basic geometry information about the Whitechapel
   game board. We start by defining some of the basic data types useful for
   modling a game *)
  
(* Whitechapel features three moves types for Jack: a regular Move, a double
   or Carriage move, and an Alleyway move*)
type move_type =
  | Move
  | Carriage
  | Alleyway

(* When played consecutively, individual moves combine to form a sequence *)
type sequence =
  | End
  | Play of move_type * sequence

(* Translates a list of moves into a sequence *)
let rec sequencify (lst : move_type list) : sequence =
  match lst with
  | [] -> End
  | hd :: tl ->
    match hd with
    | Move -> Play(Move, sequencify tl)
    | Carriage -> Play(Carriage, sequencify tl)
    | Alleyway -> Play(Alleyway, sequencify tl) ;;