let () =
  Alcotest.run "All tests"
    [ Test_priority_queue.named_suite; Test_scheduler.named_suite ]
