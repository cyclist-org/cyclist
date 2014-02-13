open Ocamlbuild_plugin ;;

(* change these to match you system *)

(* where standard .h headers for ocaml are installed *)
(* on Debian/Ubuntu these are in package ocaml-nox *)
let ocaml_headers_path = "/usr/lib/ocaml/caml"

(* base path for melt *)
(* on Debian/Ubuntu this is in package ocaml-melt *)
let ocaml_melt_lib_path = "/usr/lib/ocaml/melt"

(* base path for spot library *) 
let spot_path = "/usr/local"

let headers = 
  [ 
    "src/soundness/proof_aut.hpp"; 
    "src/soundness/proof.hpp"; 
    "src/soundness/trace.hpp" 
  ] 

let spot_include_path = spot_path ^ "/include/spot"
let spot_lib_path = spot_path ^ "/lib/spot"

let _ = dispatch begin function
  | After_rules ->
    ocaml_lib ~extern:true ~dir:ocaml_melt_lib_path "latex";

    flag ["link"; "ocaml"; "use_libsoundness"]
          (S[A"-ccopt"; A"-Lsrc/soundness"; A"-cclib"; A"-lsoundness"]);
    
    flag ["link"; "ocaml"; "use_spot"]
          (S[
            A"-ccopt"; A("-L" ^ spot_lib_path); 
            A"-cclib"; A"-lbdd";
            A"-cclib"; A"-lspot";
            A"-cclib"; A"-lstdc++"
            ]);
    
    (* the path to the .a file is necessary *)
    dep ["link"; "ocaml"; "use_libsoundness"] ["src/soundness/libsoundness.a"];
    
    flag ["c"; "compile"]  
      (S[
        A"-ccopt"; A"-xc++"; A"-ccopt"; A"-std=c++11"; 
        A"-ccopt"; A("-I" ^ ocaml_headers_path);
        A"-ccopt"; A("-I" ^ spot_include_path)]);
    
    dep  ["compile"; "c"] headers;
  | _ -> ()
end
