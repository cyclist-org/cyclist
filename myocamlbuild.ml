open Ocamlbuild_plugin ;;
 
let _ = dispatch begin function
  | After_rules ->
    ocaml_lib ~extern:true ~dir:"/usr/lib/ocaml/melt" "latex";

(*    flag ["ocaml"; "link"; "use_soundness"]
      (S[A"-cc"; A"g++"; A"-ccopt"; A"-L/home/nikos/Work/cyclist/src/soundness"; A"-custom"; A"-cclib"; A"-Isoundness"]);
    dep ["link"; "ocaml"; "use_soundness"] ["libsoundness.a"];*)

  | _ -> ()
end
