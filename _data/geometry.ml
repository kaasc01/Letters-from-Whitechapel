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
   game board. We start by defining some data types useful for modeling
   vertices and edges in a connected graph. *)
   
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
  | Carriage
  | Alleyway
  | Step