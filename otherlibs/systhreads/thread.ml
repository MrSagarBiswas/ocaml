(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Xavier Leroy and Pascal Cuoq, projet Cristal, INRIA Rocquencourt     *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* User-level threads *)

type t

external thread_initialize : unit -> unit = "caml_thread_initialize"
external thread_cleanup : unit -> unit = "caml_thread_cleanup"
external thread_new : (unit -> unit) -> t = "caml_thread_new"
external thread_uncaught_exception : exn -> unit =
            "caml_thread_uncaught_exception"

external yield : unit -> unit = "caml_thread_yield"
external self : unit -> t = "caml_thread_self" [@@noalloc]
external id : t -> int = "caml_thread_id" [@@noalloc]
external join : t -> unit = "caml_thread_join"

(* For new, make sure the function passed to thread_new never
   raises an exception. *)

let[@inline never] check_memprof_cb () = ref ()

let default_uncaught_exception_handler = thread_uncaught_exception

let uncaught_exception_handler = ref default_uncaught_exception_handler

let set_uncaught_exception_handler fn = uncaught_exception_handler := fn

exception Exit

let spawn_finaliser_thread finalise k =
  ignore (thread_new (fun () ->
    try finalise k with _ -> ()))

let deep_finalise_sync : type a b. (a, b) Effect.Deep.continuation -> unit =
  fun k ->
    match Effect.Deep.discontinue k Effect.Continuation_deadlocked with
    | _v -> ()
    | exception Effect.Continuation_deadlocked -> ()
    | exception _e -> ()
    | effect _e, _k' -> ()

let shallow_finalise_sync : type a b. (a, b) Effect.Shallow.continuation -> unit =
  fun k ->
    Effect.Shallow.discontinue_with k Effect.Continuation_deadlocked
      { retc = (fun _ -> ());
        exnc = (fun e ->
          match e with
          | Effect.Continuation_deadlocked -> ()
          | _ -> ());
        effc = (fun _ -> None) }

let offload_deep_finalise k =
  spawn_finaliser_thread deep_finalise_sync k

let offload_shallow_finalise k =
  spawn_finaliser_thread shallow_finalise_sync k

let create fn arg =
  thread_new
    (fun () ->
      try
        fn arg;
        ignore (Sys.opaque_identity (check_memprof_cb ()))
      with
      | Exit ->
        ignore (Sys.opaque_identity (check_memprof_cb ()))
      | exn ->
        let raw_backtrace = Printexc.get_raw_backtrace () in
        flush stdout; flush stderr;
        try
          !uncaught_exception_handler exn
        with
        | Exit -> ()
        | exn' ->
          Printf.eprintf
            "Thread %d killed on uncaught exception %s\n"
            (id (self ())) (Printexc.to_string exn);
          Printexc.print_raw_backtrace stderr raw_backtrace;
          Printf.eprintf
            "Thread %d uncaught exception handler raised %s\n"
            (id (self ())) (Printexc.to_string exn');
          Printexc.print_backtrace stdout;
          flush stderr)

let exit () =
  raise Exit

(* Initialization of the scheduler *)

let () =
  thread_initialize ();
  Callback.register "Thread.continuation_finalise_deep" offload_deep_finalise;
  Callback.register "Thread.continuation_finalise_shallow"
    offload_shallow_finalise;
  (* Called back in [caml_shutdown], when the last domain exits. *)
  Callback.register "Thread.at_shutdown" thread_cleanup

(* Wait functions *)

let delay = Unix.sleepf

let wait_timed_read fd d =
  match Unix.select [fd] [] [] d with ([], _, _) -> false | (_, _, _) -> true
let wait_timed_write fd d =
  match Unix.select [] [fd] [] d with (_, [], _) -> false | (_, _, _) -> true
let select = Unix.select

let wait_pid p = Unix.waitpid [] p

let sigmask = Unix.sigprocmask
let wait_signal = Unix.sigwait

external set_current_thread_name : string -> unit =
        "caml_set_current_thread_name"
