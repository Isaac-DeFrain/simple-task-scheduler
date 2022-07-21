open Simple_task_scheduler.Scheduler

(* TODO *)

let start_at_time_zero =
  let open QCheck in
  Test.make ~count:100 ~name:"schedulers should start at time t = 0" (1 -- 10)
    (fun delay ->
      let t = init ~delay [] in
      time t = 0)

let suite = List.map QCheck_alcotest.to_alcotest [ start_at_time_zero ]

let named_suite = (name, suite)
