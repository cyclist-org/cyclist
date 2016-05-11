let run name tst =
  let bt = Printexc.backtrace_status () in
  let () = Printexc.record_backtrace true in
  try
    tst ()
  with 
  | Assert_failure(_, line, _) ->
    begin
      Printf.eprintf "Test `%s' failed, line %d.\n" name line ;
      exit (-1)
    end
  | exn ->
    begin
      let sep = String.make 72 '=' in
      Printf.eprintf 
        "Test `%s' threw exception `%s'.\nBacktrace:\n%s\n%s%s\n" 
        name  (Printexc.to_string exn) sep (Printexc.get_backtrace ()) sep;
      let () = exit (-1) in ()
    end ;
  Printexc.record_backtrace bt

    
