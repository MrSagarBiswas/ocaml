(* TEST *)

open Effect
open Effect.Deep

let _ = ignore Effect.Continuation_deadlocked

type _ Effect.t += Yield : unit Effect.t

let print str =
  print_endline str

let lock = Mutex.create ()

let task1 () =
  Fun.protect
    ~finally:(fun () ->
      Mutex.lock lock;
      print "Inside task1 finally block Lock";
      Mutex.unlock lock)
    (fun () ->
      print "Task1 Started";
      perform Yield)

let task2 () =
  Mutex.lock lock;
  print "Inside task2 Lock";
  print "Major GC Starting";
  Gc.full_major ();
  print "Major GC Finished";
  Mutex.unlock lock;
  Gc.safe_point ()

let () =
  (match task1 () with
   | () -> ()
   | effect Yield, k -> ignore k);
  task2 ()
