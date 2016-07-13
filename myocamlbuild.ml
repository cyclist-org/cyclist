(* I HATE OCAMLBUILD !!!!! *)

open Ocamlbuild_plugin ;;

(* change these to match your system *)

(* where standard .h headers for ocaml are installed *)
(* on Debian/Ubuntu these are in package ocaml-nox *)
let ocaml_headers_path = "/usr/lib/ocaml/caml"

(* base path for melt *)
(* on Debian/Ubuntu this is in package ocaml-melt *)
let ocaml_melt_lib_path = "/usr/lib/ocaml/melt"

(* base path for pcre *)
(* on Debian/Ubuntu this is in package libpcre-ocaml-dev *)
let ocaml_pcre_lib_path = "/usr/lib/ocaml/pcre"

(* base path for spot library *) 
let spot_path = "/usr/local"

(* these have to be manually specified as ocamlbuild will not automatically *)
(* recognise them *)
let headers = 
  [ 
    "src/soundness/proof_aut.hpp"; 
    "src/soundness/proof.hpp"; 
    "src/soundness/trace.hpp" 
  ] 

(* you shouldn't normally have to change anything below this point *)
let spot_include_path = spot_path ^ "/include"

let _ = dispatch begin function
  | After_rules ->
    (* declare melt as external OCaml library *)
    ocaml_lib ~extern:true ~dir:ocaml_melt_lib_path "latex";
    ocaml_lib ~extern:true ~dir:ocaml_pcre_lib_path "pcre";
    
    (* how to compile "c" files, really C++ *)
    dep  ["c"; "compile"] headers;
    flag ["c"; "compile"]  
      (S[
        A"-ccopt"; A"-xc++"; A"-ccopt"; A"-std=c++0x"; 
        A"-ccopt"; A("-I" ^ ocaml_headers_path);
        A"-ccopt"; A("-I" ^ spot_include_path)]);
    
    (* declare dependencies for things using libsoundness *)
    dep ["link"; "ocaml"; "use_libsoundness"] 
      [
        (* the relative path to the .a file is necessary *)
        "src/soundness/libsoundness.a";
        "libspot.a";
        "libbddx.a";
      ];
    (* how to link everything together that uses libsoundness *)
    flag ["link"; "ocaml"; "use_libsoundness"]
      (S[
        (* following option is a workaround for a bug in ocaml/ocamlbuild 3.x *)
        (* that reorders linker arguments *)
        (* A"-cclib"; A"-Wl,--no-as-needed"; *)
        A"-cclib"; A"-lstdc++";
        A"src/soundness/libsoundness.a";
        A"libspot.a";
        A"libbddx.a";
        ]);

    (* symbolically link in place the two static libraries needed for spot *)
    (* this is done because we want to statically link them in *)
    (* and ocamlbuild will just not let me do that with external paths *)
    rule "libspot"
      ~prod:"libspot.a"
      (fun env _ -> Cmd(S[P"ln"; A"-s"; A(spot_path ^ "/lib/libspot.a"); A"."]));

    rule "libbdd"
      ~prod:"libbddx.a"
      (fun env _ -> Cmd(S[P"ln"; A"-s"; A(spot_path ^ "/lib/libbddx.a"); A"."]));
    
    flag ["link"; "ocaml"; "byte"] (A"-custom");
    
    (* skip asserts if the tag noassert is found *)
    flag ["compile"; "ocaml"; "noassert"] (A"-noassert");
  | _ -> ()
end
