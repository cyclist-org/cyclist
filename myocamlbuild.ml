open Ocamlbuild_plugin ;;

let ext_obj = !Options.ext_obj;;
let x_o = "%"-.-ext_obj;;

(* rule "ocaml C++ stubs: cpp -> o"                                                                      *)
(*   ~prod:x_o                                                                                           *)
(*   ~dep:"%.cpp"                                                                                        *)
(*   begin fun env _build ->                                                                             *)
(*     let c = env "%.cpp" in                                                                            *)
(*     let o = env x_o in                                                                                *)
(*     let comp =                                                                                        *)
(*       if Tags.mem "native" (tags_of_pathname c) then !Options.ocamlopt else !Options.ocamlc in        *)
(*     let cc =                                                                                          *)
(*       Cmd(S[comp; T(tags_of_pathname c++"c"++"compile"); A"-custom"; A"-cc"; A"g++"; A"-c"; Px c]) in *)
(*     if Pathname.dirname o = Pathname.current_dir_name then cc                                         *)
(*     else Seq[cc; mv (Pathname.basename o) o]                                                          *)
(*   end;;                                                                                               *)

let _ = dispatch begin function
  | After_rules ->
    ocaml_lib ~extern:true ~dir:"/usr/lib/ocaml/melt" "latex";

    (* dep ["link"; "ocaml"; "use_soundness"] ["libsoundness.a"]; *)
  | _ -> ()
end
