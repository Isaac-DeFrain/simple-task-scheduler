(lang dune 3.3)

(name simple_task_scheduler)

(generate_opam_files true)

(source
 (github Isaac-DeFrain/simple-task-scheduler))

(authors "Isaac DeFrain")

(maintainers "Isaac DeFrain")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name simple_task_scheduler)
 (synopsis "A simple task scheduler based on a priority queue")
 (description "A longer description")
 (depends ocaml dune core)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
