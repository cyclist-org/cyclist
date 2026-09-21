module C = Configurator.V1

let package = "libspot"
let expr = "libspot >= 2.15"

let die_no_libspot msg =
  C.die
    "Cyclist requires %s. Please install Spot (https://spot.lre.epita.fr/) and \
     make sure pkg-config can find it, e.g. by setting PKG_CONFIG_PATH.\n\
     %s"
    expr msg

let () =
  C.main ~name:"libspot" (fun c ->
      let pc =
        match C.Pkg_config.get c with
        | None -> die_no_libspot "pkg-config was not found in PATH."
        | Some pc -> pc
      in
      let conf =
        match C.Pkg_config.query_expr_err pc ~package ~expr with
        | Error msg -> die_no_libspot msg
        | Ok conf -> conf
      in
      C.Flags.write_sexp "c_flags.sexp" conf.cflags;
      C.Flags.write_sexp "c_library_flags.sexp" conf.libs)
