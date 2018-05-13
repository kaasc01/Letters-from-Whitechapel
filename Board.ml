(* 
   Christian Kaas
   Letters from Whitechapel - A Game Solver
   Spring 2018
*)

(* This module provides a model for a Whitechapel-like board game, implemented
   as an adjacency hashtable *)

(* Steps to use graph library in REPL:

      #use "topfind" ;;
      #require "ocamlgraph" ;;
      open Graph ;;
      #directory "/home/christian/CS/Projects/Whitechapel" ;;
      #mod_use "Geometry.ml";;
      open Geometry;;

 *) 

open Graph
open Geometry

(* Describe vertex interface and implementation here *)
module Vertex =
  struct
    type t = node_type
    let compare = Pervasives.compare
    let hash = Hashtbl.hash
    let equal = (=)
  end

(* Describe edge interface and implementation here *)
module Edge =
  struct
    type t = move_type 
    let compare = Pervasives.compare
    let default = Move
    let equal = (=)
  end

(* Describe choice of graph implementation here *)
module type GAMEBOARD =
  sig
    type t
    val create : unit -> t
    val pull_circles : t -> Vertex.t -> Edge.t -> Vertex.t list
    val single : t -> Vertex.t -> Vertex.t list
    val carriage : t -> Vertex.t -> Vertex.t list
    val alleyway : t -> Vertex.t -> Vertex.t list
  end


module Whitechapel : GAMEBOARD =
  struct
    
    (* Implements a standard graph structure, and make all the default
       graph methods available to the Whitechapel graph *)
    module Graph = Imperative.Graph.ConcreteLabeled(Vertex)(Edge)

    (* Aliases the graph's type signature, to be consistent with the
       GAMEBOARD signature *)
    type t = Graph.t

    (* Implements a set data structure, to help keep track of visited states *)
    module S = Set.Make(struct
                          type t = Vertex.t
                          let compare = Pervasives.compare
                        end)

    (* Loads a dataset of edges into the graph g, assigning type t to the
       connections made *)
    let rec load_geometry (g : Graph.t) (t : Edge.t)
                          (dataset : (Vertex.t * Vertex.t list) list) : unit =
      match dataset with
      | [] -> ()
      | (v1, v2_lst) :: tl ->
          List.iter (fun v2 -> Graph.add_edge_e g (v1, t, v2)) v2_lst;
          load_geometry g t tl

    (* Returns the set of vertices reachable from vertex n through an edge
       of type e, in graph g. Optionally, exclude from results and values
       already present in set f *)
    let succ_e (g : Graph.t)
               (n : Vertex.t)
               (e : Edge.t)
               ?(f : S.t = S.empty) () : S.t =
      let open Graph in
      let filter_by_type acc elt =
        let (_, ctype, v2) = find_edge g n elt in
        if Edge.equal ctype e then S.add v2 acc else acc
      in
      (* For all vertices that are reachable from n in g... *)
      let reachable = succ g n in
      (* Return only those reachable through a connection of type e... *)
      let filtered = List.fold_left (filter_by_type) (S.empty) (reachable) in
      (* And return only those filtered vertices not already found in f *)
      S.diff filtered f

    (* Returns a set of Circles in graph g that are reachable within a single
       move from Circle n *)
    let pull_circles (g : Graph.t) (n : Vertex.t) (e : Edge.t) : S.t =
      let open S in
      let visited = ref (singleton n) in 
      let rec iter (pend : Vertex.t list) (result : S.t) =
        match pend with
        | [] -> result
        | hd :: tl ->
            visited := add hd !visited;
            match hd with
            | Circle _ -> iter tl (add hd result)
            | Square _ -> let next_nodes = succ_e g hd e ~f:(!visited) () in
                          iter ((elements next_nodes) @ pend) result
      in iter (elements (succ_e g n e ())) (empty)



    let make_move (g : Graph.t) (n : Vertex.t) (ct : int) : Vertex.t list =
      let rec move (result : S.t) (count : int) =
        if count < 0 then raise (Invalid_argument "make_move: invalid move")
        else if count = 0 then S.elements result
        else
          let union = S.union result (S.of_list (pull_circles g n Move)) in 
          move union (count - 1)
      in
      move (S.of_list (pull_circles g n Move)) ct 

    let single (g : Graph.t) (n : Vertex.t) : Vertex.t list =
      make_move g n 1 

    let carriage (g : Graph.t) (n : Vertex.t) : Vertex.t list =
      make_move g n 2

    let alleyway (g : Graph.t) (n : Vertex.t) : Vertex.t list =
      pull_circles g n Alleyway

    let create () : Graph.t =
      let board = Graph.create () in 
      load_geometry board Alleyway alleyways;
      load_geometry board Move moves;
      board

   end