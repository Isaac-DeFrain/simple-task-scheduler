open! Base
open! Math_ops

let name = "Priority queue"

module type Comparable = sig
  type t = Task.t * int

  val compare : t -> t -> int
end

module Comparable_task = struct
  type t = Task.t * int

  let compare (_, a) (_, b) = compare a b
end

type elem = Comparable_task.t

type t =
  { mutable capacity : int
  ; mutable size : int
  ; mutable items : Comparable_task.t array
  }

let capacity { capacity; _ } = capacity

let size { size; _ } = size

(* indicies *)
let get_left_child_idx pi = (2 * pi) + 1

let get_right_child_idx pi = (2 * pi) + 2

let get_parent_idx ci = (ci - 1) / 2

(* child + parnet predicates *)
let has_left_child i t = get_left_child_idx i < t.size

let has_right_child i t = get_right_child_idx i < t.size

let has_parent i _ = get_parent_idx i >= 0

(* child + parent items *)
let left_child i t =
  if not @@ has_left_child i t then None
  else Some t.items.(get_left_child_idx i)

let right_child i t =
  if not @@ has_right_child i t then None
  else Some t.items.(get_right_child_idx i)

let parent i t =
  if not @@ has_parent i t then None else Some t.items.(get_parent_idx i)

(* maintenance *)
let default = (Task.empty, 0)

let ensure_extra_capacity t =
  let cap = t.capacity in
  if t.size = cap then (
    let copy () =
      let open Array in
      let new_items = create ~len:(2 * cap) default in
      iteri t.items ~f:(fun i x -> new_items.(i) <- x);
      new_items
    in
    t.capacity <- 2 * cap;
    t.items <- copy ())

(* implementation *)
let is_empty t = t.size = 0

let peek t = if is_empty t then None else Some t.items.(0)

let rec bubble_up x i t =
  match parent i t with
  | None -> ()
  | Some px ->
    let pi = get_parent_idx i in
    if Comparable_task.compare x px = 1 then (
      Array.swap t.items i pi;
      bubble_up x pi t)

let push x t =
  ensure_extra_capacity t;
  let i = t.size in
  t.size <- i + 1;
  t.items.(i) <- x;
  bubble_up x i t

let rec trickle_down x i t =
  let open Comparable_task in
  match (left_child i t, right_child i t) with
  | None, None -> ()
  | Some cl, None ->
    if compare x cl < 1 then Array.swap t.items i @@ get_left_child_idx i
  | Some cl, Some cr ->
    if compare cl cr < 1 then (
      if compare x cr < 1 then (
        let cri = get_right_child_idx i in
        Array.swap t.items i cri;
        trickle_down x cri t))
    else if compare x cl < 1 then (
      let cli = get_left_child_idx i in
      Array.swap t.items i cli;
      trickle_down x cli t)
  | _ -> assert false

let pop t =
  if t.size = 0 then None
  else
    let top = t.items.(0) in
    let x = t.items.(t.size - 1) in
    t.items.(0) <- x;
    t.items.(t.size - 1) <- (Task.empty, 0);
    t.size <- t.size - 1;
    trickle_down x 0 t;
    Some top

let create ~capacity =
  { capacity; size = 0; items = Array.create ~len:capacity default }

let mk_positive_list items =
  Array.to_list items |> List.filter ~f:(fun (_, x) -> x > 0)

let to_list { items; _ } = mk_positive_list items

let to_sorted { items; _ } =
  mk_positive_list items |> List.sort ~compare:Comparable_task.compare
