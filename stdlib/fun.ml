(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         The OCaml programmers                          *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

let id x = x
let const c _ = c
let compose f g x = f (g x)
let flip f x y = f y x
let negate p v = not (p v)

exception Finally_raised of exn

let () = Printexc.register_printer @@ function
| Finally_raised exn -> Some ("Fun.Finally_raised: " ^ Printexc.to_string exn)
| _ -> None

let is_continuation_deadlocked exn =
  Printexc.exn_slot_name exn = "Stdlib.Effect.Continuation_deadlocked"

let protect ?(async_finally : (unit -> unit) option) ~(finally : unit -> unit) work =
  let finally_no_exn () =
    try finally () with e ->
      let bt = Printexc.get_raw_backtrace () in
      Printexc.raise_with_backtrace (Finally_raised e) bt
  in
  match work () with
  | result -> finally_no_exn () ; result
  | exception work_exn ->
      let work_bt = Printexc.get_raw_backtrace () in
      (match work_exn with
       | _ when is_continuation_deadlocked work_exn ->
           (* Continuation_deadlocked: only run cleanup if async_finally was provided *)
           (match async_finally with
            | Some f ->
                (try f () with e ->
                  let bt = Printexc.get_raw_backtrace () in
                  Printexc.raise_with_backtrace (Finally_raised e) bt)
            | None -> ())
       | _ -> finally_no_exn ()) ;
      Printexc.raise_with_backtrace work_exn work_bt
