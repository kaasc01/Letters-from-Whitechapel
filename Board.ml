(* Note: Commands below required for troubleshooting in REPL:
      #use "topfind" ;;
      #require "ocamlgraph" ;;
      open Graph ;;
      #directory "/home/christian/CS/Projects/Whitechapel/_data" ;;
      #mod_use "geometry.ml";;
      open Geometry;;
      #mod_use "moves.ml";;
      open Moves;;
      #mod_use "carriages.ml";;
      open Carriages;;
      #mod_use "alleyways.ml";;
      open Alleyways;;
 *) 


open Geometry

open Moves
open Carriages
open Alleyways

let move_table = Hashtbl.create 195 ;;
let carriage_table = Hashtbl.create 195;;
let alleyway_table = Hashtbl.create 195;;

module S = Set.Make(struct
                      type t = int
                      let compare = Pervasives.compare
                    end) ;;

let populate_table (tbl : ('a, 'a list) Hashtbl.t)
                   (lst : (node_type * node_type list) list ) : unit list =
  let open List in
  let strip_elt (elt : node_type) : int =
    match elt with
    | Circle n -> n
    | _ -> raise (Invalid_argument "strip_type : unexpected match")
  in
  let strip_list (lst : node_type list) : int list =
    map strip_elt lst
  in
  map (fun (v, vlist) ->
        Hashtbl.add tbl (strip_elt v) (strip_list vlist)
      ) lst ;;

let union lst1 lst2 =
  let open List in
  let merged = merge compare lst1 lst2 in
  sort_uniq compare merged ;;

populate_table move_table moves ;;
populate_table carriage_table carriages ;;
populate_table alleyway_table alleyways ;;

type sequence =
  | End
  | Play of move_type * sequence ;;

(* Converts a list of moves types into a sequence *)
let rec sequencify (lst : move_type list) : sequence =
  match lst with
  | [] -> End
  | hd :: tl ->
    match hd with
    | Move -> Play(Move, sequencify tl)
    | Carriage -> Play(Carriage, sequencify tl)
    | Alleyway -> Play(Alleyway, sequencify tl)
    | _ -> raise (Invalid_argument "sequencify : unexpected match");;

(* Given a move type, returns the appropriate lookup table to use*)
let map_move_to_table (m : move_type) : ('a, 'a list) Hashtbl.t =
  match m with
  | Move -> move_table
  | Carriage -> carriage_table
  | Alleyway -> alleyway_table
  | _ -> raise (Invalid_argument "map_move_to_table : unexpected match") ;;

(* Filters a target list against a target list -- returns only values NOT
   found in the filter list *)
let list_filter (target_lst : int list) (filter_lst : int list) =
  let open List in
  filter (fun elt -> not (mem elt filter_lst)) target_lst ;;

(* Given a sequence of moves and an initial position, asserts whether any of 
   the members of the target list can be reached by playing out the sequence *)
let rec test_me (s : sequence) (initial : int)
                (visited : int list) (unvisited : int list) : bool =
  match s with
  | End -> false
  | Play (this_move, next) ->
      let open List in
      (* Identify the right lookup table based on the move*)
      let this_table = map_move_to_table this_move in
      (* Pull the list of all possible nodes from current position*)
      let all_moves = Hashtbl.find this_table initial in
      (* Filter possible nodes for any we know were not visited *)
      let no_unvisit = list_filter all_moves unvisited in
      (* If any of the possible nodes are visited, then true *)
      if exists (fun elt -> mem elt visited) no_unvisit then true else
      (* Else, repeat the same test procedure for all subsequent moves *)
        let check_next =
          map (fun n -> test_me next n visited unvisited) no_unvisit
        in
        (* Return true if at least one of the following moves allows access
           to any of the visited nodes *)
        exists ((=) true) check_next ;;

let rec audit_sequence (s : sequence) (init : int)
                       (visited : int list) (unvisited : int list) =
  match s with
  | End ->
  | Play (this_move, next) ->


(* 158 -> C160 -> M161 -> C104 -> M87 -> C69 -> M68 -> M53 -> M67 *)
let night1_moves = [160; 161; 104; 87; 69; 68; 53];;
let night1 = sequencify [Carriage; Move; Carriage; Move; Carriage; Move; Move; Move];;
(* 21 -> M20 -> M18 -> M17 -> C33 -> C52 -> M67*)
let night2_moves = [20; 18; 17; 33; 52];;
let night2 = sequencify [Move; Move; Move; Carriage; Carriage; Move];;
(* 147 -> C145 -> M144 -> M143 -> M102 -> M68 -> M53 -> M67*)
let night3_moves = [145; 144; 143; 102; 68; 53];;
let night3 = sequencify [Carriage; Move; Move; Move; Move; Move; Move];;
(* 27 -> M79 -> C117 -> C120 -> M99 -> M84 -> M67*)
let night4_moves = [79; 117; 120; 99; 84];;
let night4 = sequencify [Move; Carriage; Carriage; Move; Move; Move];;
(* 65 -> M82 -> M62 -> M48 -> M28 -> C10 -> M30 -> M50 -> M52 -> M67*)
let night5_moves = [82; 62; 48; 28; 10; 30; 50; 52];;
let night5 = sequencify [Move; Move; Move; Move; Carriage; Move; Move; Move; Move];;

let n1p = generate_space night1 158 (elim_gen (5 * (List.length night1_moves)) night1_moves);;
let n2p = generate_space night2 21 (elim_gen (5 * (List.length night2_moves)) night2_moves);;
let n3p = generate_space night3 147 (elim_gen (5 * (List.length night3_moves)) night3_moves);;
let n4p = generate_space night4 27 (elim_gen (5 * (List.length night4_moves)) night4_moves);;
let n5p = generate_space night5 65 (elim_gen (5 * (List.length night5_moves)) night5_moves);;

let reduce (lst : int list list) =
  let set_e = List.init 195 (fun x -> x + 1) in
  let search_space = ref (S.of_list set_e) in
  List.iter (fun elt -> search_space := S.inter (S.of_list elt) (!search_space)) lst;
  S.elements (!search_space);;

let elim_gen (n : int) (lst : int list) =
  let rec iter (n : int) (lst : int list) =
    if n = 0 then lst 
    else iter (n - 1) ((Random.int 195 + 1) :: lst)
  in
  let rec truncate (n : int) (build_down : int list) (build_up : int list) =
    match build_down with
    | [] -> []
    | hd :: tl -> if (n = 0) then build_up
                  else truncate (n - 1) tl (hd :: build_up)
  in
  let rands = List.filter (fun x -> not (List.mem x lst)) (iter 100 []) in
  truncate n rands [] ;;