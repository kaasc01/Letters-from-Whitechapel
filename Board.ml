(* Note: Commands below required for troubleshooting in REPL:
      #use "topfind" ;;
      #require "ocamlgraph" ;;
      #require "unix";;
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
      #mod_use "testnetwork.ml";;
      open Testnetwork;;
 *) 


open Geometry

open Moves
open Carriages
open Alleyways

(* Create rapid lookup tables *)
let move_table = Hashtbl.create 195 ;;
let carriage_table = Hashtbl.create 195;;
let alleyway_table = Hashtbl.create 195;;
let test_table = Hashtbl.create 13;;

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

populate_table move_table moves ;;
populate_table carriage_table carriages ;;
populate_table alleyway_table alleyways ;;
populate_table test_table simple_bi ;;

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

let evaluate_sequence_logger (s : sequence) (initial_position : int)
                      (must_visit : int list) (do_not_visit : int list)
                     : int list =
  let logger = Hashtbl.create 100000 in
  let rec eval_move (s : sequence) (initial_position : int)
                    (must_visit : int list) (possible_position : int list)
                   : int list =
    match s with
    | End -> if must_visit = [] then possible_position else []
    | Play (this_move, next) ->
        let open List in
        (* Identify the right lookup table based on the move*)
        let this_table = map_move_to_table this_move in (**)
        (* Pull the list of all possible nodes from current position*)
        let all_moves = Hashtbl.find this_table initial_position in
        (* Filter possible nodes for any we know were not visited *)
        let no_unvisit = list_filter all_moves do_not_visit in
        (* Filters an element elt out of the visited list -- returns visited
           unchanged if elt isn't present *)
        let remove_visited elt = filter ((<>) elt) must_visit in  
        merge_all (map (
                 fun elt ->
                  try
                    (* Try to find our move combo in the log. If it is already
                       done then terminate this branch *)
                    Hashtbl.find logger (elt, next, must_visit);
                    []
                  with
                  (* If the logger doesn have this branch yet, then add it to the log
                     and proceeed as before -- tuple of this element and remaining moves *)
                  | _ -> Hashtbl.add logger (elt, next, must_visit) [];
                         eval_move next elt (remove_visited elt) [elt]
         
               ) no_unvisit)
  in
  eval_move s initial_position must_visit [] ;;





(* Helper function to shuffle a list randomly *)
let list_shuffle (lst : int list) : int list =
  let open List in
  let nd = map (fun c -> (Random.bits (), c)) lst in
  let sond = sort compare nd in
  map snd sond ;;

(* A list of random numbers between 1 and 195 *)
let board_circles = List.init 195 (fun x -> x + 1)

(* Returns the first n elements of a list lst *)
let rec list_first_n (n : int) (lst : int list) : int list =
  match lst with
  | [] -> failwith "firstk"
  | hd :: tl -> if n = 1 then [hd] else hd :: list_first_n (n - 1) tl ;;

(* Generates a list of random "did not visit" values to simulate gameplay --
   start by excluding the circles we know to have been visited, then shuffle,
   before taking the first n values *)
let get_dnv (excl : int list) (n : int) : int list =
  let excluded_list = list_filter board_circles excl in
  let shuffled = list_shuffle excluded_list in
  list_first_n n shuffled ;;

let gUESSES = 5 ;; (* Guesses per night for the detective player *)

(* This is the location of the night1 murder *)
let night1_origin = 158 ;;
(* These are the moves Jack makes on night1 *)
let night1_bak = sequencify [Carriage; Move];;
let night1 = sequencify [Carriage; Move; Carriage; Move; Carriage; Move; Move; Move];;
(* These are the actual cells visited, which the solver does not know *)
(* 158 -> C160 -> M161 -> C104 -> M87 -> C69 -> M68 -> M53 -> M67 *)
let night1_moves = [160; 161; 104; 87; 69; 68; 53; 67] ;;
(* These are randomly generated "did not visit" values to approximate game play --
   first exclude the ones we know from the list*)
let night1_dnv = get_dnv night1_moves ((List.length night1_moves + 1) * gUESSES) 
(* These are the locations we discovered Jack to have been during this round *)
let night1_visited = [104; 69]


(* This is the location of the night2 murder *)
let night2_origin = 21 ;;
(* These are the moves Jack makes on night2 *)
let night2 = sequencify [Move; Move; Move; Carriage; Carriage; Move];;
(* These are the actual cells visited, which the solver does not know *)
(* 21 -> M20 -> M18 -> M17 -> C33 -> C52 -> M67*)
let night2_moves = [20; 18; 17; 33; 52; 67];;
(* These are randomly generated "did not visit" values to approximate game play --
   first exclude the ones we know from the list*)
let night2_dnv = get_dnv night2_moves ((List.length night2_moves + 1) * gUESSES) 
(* These are the locations we discovered Jack to have been during this round *)
let night2_visited = [17]

(* This is the location of the night3 murder *)
let night3_origin = 147 ;;
(* These are the moves Jack makes on night3 *)
let night3 = sequencify [Carriage; Move; Move; Move; Move; Move; Move];;
(* These are the actual cells visited, which the solver does not know *)
(* 147 -> C145 -> M144 -> M143 -> M102 -> M68 -> M53 -> M67*)
let night3_moves = [145; 144; 143; 102; 68; 53; 67];;
(* These are randomly generated "did not visit" values to approximate game play --
   first exclude the ones we know from the list*)
let night3_dnv = get_dnv night3_moves ((List.length night3_moves + 1) * gUESSES) 
(* These are the locations we discovered Jack to have been during this round *)
let night3_visited = [144; 68]

(* This is the location of the night4 murder *)
let night4_origin = 27 ;;
(* These are the moves Jack makes on night4 *)
let night4 = sequencify [Move; Carriage; Carriage; Move; Move; Move];;
(* These are the actual cells visited, which the solver does not know *)
(* 27 -> M79 -> C117 -> C120 -> M99 -> M84 -> M67*)
let night4_moves = [79; 117; 120; 99; 84; 67];;
(* These are randomly generated "did not visit" values to approximate game play --
   first exclude the ones we know from the list*)
let night4_dnv = get_dnv night4_moves ((List.length night4_moves + 1) * gUESSES) 
(* These are the locations we discovered Jack to have been during this round *)
let night4_visited = []

(* This is the location of the night5 murder *)
let night5_origin = 65 ;;
(* These are the moves Jack makes on night5 *)
let night5 = sequencify [Move; Move; Move; Move; Carriage; Move; Move; Move; Move];;
(* These are the actual cells visited, which the solver does not know *)
(* 65 -> M82 -> M62 -> M48 -> M28 -> C10 -> M30 -> M50 -> M52 -> M67*)
let night5_moves = [82; 62; 48; 28; 10; 30; 50; 52; 67];;
(* These are randomly generated "did not visit" values to approximate game play --
   first exclude the ones we know from the list*)
let night5_dnv = get_dnv night5_moves ((List.length night5_moves + 1) * gUESSES) 
(* These are the locations we discovered Jack to have been during this round *)
let night5_visited = [28; 10; 30]


let call_timed (f : 'a -> 'b) (x : 'a) : 'b * float =
  let t0 = Unix.gettimeofday() in 
  let result = f x in 
  let t1 = Unix.gettimeofday() in
  (result, t1 -. t0) ;;

(* call_reporting_time f x -- Applies f to x returning the result,
   reporting timing information on stdout as a side effect. *)
  
let call_reporting_time (f : 'a -> 'b) (x : 'a) : 'b =
  let result, time = call_timed f x in
  Printf.printf "time (msecs): %f\n" (time *. 1000.);
  result ;;


let night1_search_space () = evaluate_sequence_logger night1 night1_origin night1_visited night1_dnv ;;
let night2_search_space () = evaluate_sequence_logger night2 night2_origin night2_visited night2_dnv ;;
let night3_search_space () = evaluate_sequence_logger night3 night3_origin night3_visited night3_dnv ;;
let night4_search_space () = evaluate_sequence_logger night4 night4_origin night4_visited night4_dnv ;;
let night5_search_space () = evaluate_sequence_logger night5 night5_origin night4_visited night5_dnv ;;

let n1 = call_reporting_time night1_search_space ();;
let n2 = call_reporting_time night2_search_space ();;
let n3 = call_reporting_time night3_search_space ();;
let n4 = call_reporting_time night4_search_space ();;
let n5 = call_reporting_time night5_search_space ();;

let intersect (lst : int list list) : int list =

let module S = Set.Make(struct
                      type t = int
                      let compare = Pervasives.compare
                    end) in

  let rec converter (lst : int list list) : S.t = 
    match lst with
    | [] -> S.of_list board_circles
    | hd :: tl -> S.inter (S.of_list hd) (converter tl)
  in S.elements (converter lst) ;;