open! Base

let name = "Scheduler"

type t =
  { id : int
  ; delay : int
  ; heap : Priority_queue.t
  ; mutable time : int
  ; (* task * count * ready_time *)
    queue : (Task.t * int * int) Queue.t
  ; mutable schedule : scheduled list
  }

and scheduled =
  | Idle
  | Task of Task.t

let to_string s =
  String.concat ~sep:" >> "
  @@ List.map
       ~f:(function
         | Idle -> "idle"
         | Task x -> Task.to_string x)
       s

(* hidden id generator *)
let _id = ref (-1)

let id t = t.id

let time t = t.time

let heap t =
  Priority_queue.to_list t.heap
  |> List.map ~f:(fun (a, b) -> (Task.to_string a, b))

let heap_size t = Priority_queue.size t.heap

let heap_peek t =
  Option.map ~f:(fun (x, c) -> (Task.to_string x, c))
  @@ Priority_queue.peek t.heap

let queue t = t.queue

let queue_size t = Queue.length t.queue

let print_schedule t = Caml.Printf.printf "%s/n" @@ to_string t.schedule

let create delay =
  { id =
      (Int.incr _id;
       !_id)
  ; delay
  ; time = 0
  ; heap = Priority_queue.create ~capacity:13
  ; queue = Queue.create ()
  ; schedule = []
  }

let init ~delay tasks =
  let t = create delay in
  let tbl = Hashtbl.create (module Char) in
  List.iter tasks ~f:(fun x ->
      let x = Task.to_char x in
      match Hashtbl.find tbl x with
      | None -> Hashtbl.add_exn tbl ~key:x ~data:1
      | Some c ->
        Hashtbl.(
          remove tbl x;
          add_exn tbl ~key:x ~data:(c + 1)));
  let alist = Hashtbl.to_alist tbl in
  let open List in
  iter alist ~f:(fun (x, c) ->
      if c > 1 then Priority_queue.push (Task.of_char x, c) t.heap);
  t

(* runs the scheduled tasks in order from most to least prevelant *)
let run t =
  let rec heap_check () =
    t.time <- t.time + 1;
    match Priority_queue.pop t.heap with
    | None ->
      if not @@ Queue.is_empty t.queue then t.schedule <- Idle :: t.schedule;
      queue_check ()
    | Some (task, count) ->
      t.schedule <- Task task :: t.schedule;
      if count > 1 then Queue.enqueue t.queue (task, count - 1, t.time + t.delay);
      queue_check ()
  and queue_check () =
    match Queue.peek t.queue with
    | None ->
      t.schedule <- List.rev t.schedule;
      Caml.Printf.printf "finished at time: %d\n" t.time;
      print_schedule t
    | Some (task, count, ready_time) ->
      (if ready_time <= t.time then
       let _ = Queue.dequeue t.queue in
       Priority_queue.push (task, count) t.heap);
      heap_check ()
  in
  heap_check ()
