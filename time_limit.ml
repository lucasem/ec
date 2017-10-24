(* I hate Core *)

open Unix

exception Timeout
let sigalrm_handler = Sys.Signal_handle (fun _ -> raise Timeout)
let run_for_interval (time : float) (c : unit -> 'a option) : 'a option =
  let old_behavior = Sys.signal Sys.sigalrm sigalrm_handler in
   let reset_sigalrm () = Sys.set_signal Sys.sigalrm old_behavior
   in ignore (Unix.setitimer ITIMER_REAL {it_interval = 0.0; it_value = time}) ;
      try
        let res = c () in
        ignore (Unix.setitimer ITIMER_REAL {it_interval = 0.0; it_value = 0.0}) ;
        reset_sigalrm () ; res
      with _ -> begin
        reset_sigalrm ();
        ignore (Unix.setitimer ITIMER_REAL {it_interval = 0.0; it_value = 0.0}) ;
        None
      end
