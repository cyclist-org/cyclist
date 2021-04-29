open Lib
open   Symbols
open   Parsers

open Generic
open Seplog

open While
(* open Program *)

open MParser

let termination = ref false

module Field = Program.Field
module Cond = Program.Cond
(* module Cmd = Program.Cmd *)


exception WrongCmd

let is_prog_var v = Term.is_free_var v

let is_prog_term t = Term.is_nil t || is_prog_var t


module Cmd = struct
  type cmd_t =
    | Stop
    | Return
    | Skip
    | Assign of Term.t * Term.t
    | Load of Term.t * Term.t * Field.t
    | Store of Term.t * Field.t * Term.t
    | New of Term.t
    | Free of Term.t
    | If of Cond.t * t
    | IfElse of Cond.t * t * t
    | While of Cond.t * t
    | ProcCall of string * Term.FList.t
    | Assert of Form.t
    | Parallel of t * t

  and basic_t = {label: int option; cmd: cmd_t}

  and t = basic_t list

  let get_cmd c = match c with [] -> raise WrongCmd | hd :: _ -> hd.cmd

  let get_cont c = match c with [] -> raise WrongCmd | _ :: tl -> tl

  let split c =
    match c with [] | [_] -> raise WrongCmd | hd :: tl -> ([hd], tl)

  let is_empty c = Blist.is_empty c

  let is_not_empty c = not (is_empty c)

  let is_assign c =
    is_not_empty c && match get_cmd c with Assign _ -> true | _ -> false

  let is_load c =
    is_not_empty c && match get_cmd c with Load _ -> true | _ -> false

  let is_store c =
    is_not_empty c && match get_cmd c with Store _ -> true | _ -> false

  let is_new c =
    is_not_empty c && match get_cmd c with New _ -> true | _ -> false

  let is_free c =
    is_not_empty c && match get_cmd c with Free _ -> true | _ -> false

  let is_stop c =
    is_not_empty c && match get_cmd c with Stop -> true | _ -> false

  let is_return c =
    is_not_empty c && match get_cmd c with Return -> true | _ -> false

  let is_skip c =
    is_not_empty c && match get_cmd c with Skip -> true | _ -> false

  let is_proc_call c =
    is_not_empty c
    && match get_cmd c with ProcCall (_, _) -> true | _ -> false

  let is_assert c =
    is_not_empty c && match get_cmd c with Assert _ -> true | _ -> false

  let is_basic c =
    is_not_empty c
    &&
    match get_cmd c with
    | ProcCall (_, _)
     |Assign _ | Load _ | Store _ | New _ | Free _ | Stop | Skip ->
        true
    | _ -> false

  let is_final = Fun.list_disj [is_empty; is_stop; is_return]

  let is_if c =
    is_not_empty c && match get_cmd c with If _ -> true | _ -> false

  let is_ifelse c =
    is_not_empty c && match get_cmd c with IfElse _ -> true | _ -> false

  let is_while c =
    is_not_empty c && match get_cmd c with While _ -> true | _ -> false

  let is_parallel c = 
    is_not_empty c && match get_cmd c with Parallel _ -> true | _ -> false

  let mklc c = {label= None; cmd= c}

  let mk_basic c = [{label= None; cmd= c}]

  let mk_assign x e = mk_basic (Assign (x, e))

  let mk_load x e s = mk_basic (Load (x, e, s))

  let mk_store e1 s e2 = mk_basic (Store (e1, s, e2))

  let mk_new x = mk_basic (New x)

  let mk_free e = mk_basic (Free e)

  let mk_stop = mk_basic Stop

  let mk_return = mk_basic Return

  let mk_skip = mk_basic Skip

  let mk_proc_call p args = mk_basic (ProcCall (p, args))

  let mk_assert f = mk_basic (Assert f)

  let mk_if cond cmd = mk_basic (If (cond, cmd))

  let mk_ifelse cond cmd cmd' = mk_basic (IfElse (cond, cmd, cmd'))

  let mk_while cond cmd = mk_basic (While (cond, cmd))

  let mk_seq cmd cmd' = cmd @ cmd'

  let mk_parallel cond cmd = cmd || cmd 

  let mk_from_list l = Blist.flatten l

  let rec parse_cmd_opt st =
    ( attempt (parse_symb keyw_stop >>$ Some Stop)
    <|> attempt (parse_symb keyw_return >>$ Some Return)
    <|> attempt (parse_symb keyw_skip >>$ Some Skip)
    <|> attempt
          ( parse_symb keyw_assert
          >> Tokens.parens Form.parse
          |>> fun f -> Some (Assert f) )
    <|> attempt
          ( parse_symb keyw_free
          >> Tokens.parens Term.parse
          |>> fun v ->
          assert (is_prog_var v) ;
          Some (Free v) )
    <|> attempt
          ( parse_symb keyw_if >> Cond.parse
          >>= fun cond ->
          parse_symb keyw_then >> parse
          >>= fun cmd1 ->
          parse_symb keyw_else >> parse
          >>= fun cmd2 ->
          parse_symb keyw_fi >>$ Some (IfElse (cond, cmd1, cmd2)) )
    <|> attempt
          ( parse_symb keyw_if >> Cond.parse
          >>= fun cond ->
          parse_symb keyw_then >> parse
          >>= fun cmd -> parse_symb keyw_fi >>$ Some (If (cond, cmd)) )
    <|> attempt
          ( parse_symb keyw_while >> Cond.parse
          >>= fun cond ->
          parse_symb keyw_do >> parse
          >>= fun cmd -> parse_symb keyw_od >>$ Some (While (cond, cmd)) )
    <|> attempt
          ( parse_symb symb_lp >> parse
          >>= fun cmd1 ->
          parse_symb symb_parallel >> parse
          >>= fun cmd2 -> 
          parse_symb symb_rp >>$ Some (Parallel (cmd1, cmd2)) )
    (*   | v = var; FLD_SEL; fld = IDENT; ASSIGN; t = term *)
    <|> attempt
          ( parse_ident
          >>= fun v ->
          parse_symb symb_fld_sel >> parse_ident
          >>= fun id ->
          parse_symb symb_assign >> Term.parse
          |>> fun t ->
          let v = Term.of_string v in
          assert (is_prog_var v) ;
          Some (Store (v, id, t)) )
    (*   v = var; ASSIGN; NEW; LP; RP { P.Cmd.mk_new v } *)
    <|> attempt
          ( parse_ident << parse_symb symb_assign << parse_symb keyw_new
          << parse_symb symb_lp << parse_symb symb_rp
          |>> fun v ->
          let v = Term.of_string v in
          assert (is_prog_var v) ;
          Some (New v) )
    (*   | v1 = var; ASSIGN; v2 = var; FLD_SEL; fld = IDENT *)
    <|> attempt
          ( parse_ident
          >>= fun v1 ->
          parse_symb symb_assign >> parse_ident
          >>= fun v2 ->
          parse_symb symb_fld_sel >> parse_ident
          |>> fun id ->
          let v1 = Term.of_string v1 in
          let v2 = Term.of_string v2 in
          assert (is_prog_var v1 && is_prog_var v2) ;
          Some (Load (v1, v2, id)) )
    (* | v = var; ASSIGN; t = term { P.Cmd.mk_assign v t } *)
    <|> attempt
          ( parse_ident
          >>= fun v ->
          parse_symb symb_assign >> parse_ident
          |>> fun t ->
          let v = Term.of_string v in
          let t = Term.of_string t in
          assert (is_prog_var v) ;
          Some (Assign (v, t)) )
    <|> attempt
          ( parse_ident
          >>= fun p ->
          Tokens.parens (Tokens.comma_sep Term.parse)
          |>> fun args ->
          Blist.iter
            (fun arg -> assert (is_prog_var arg || Term.is_nil arg))
            args ;
          Some (ProcCall (p, args)) )
    <|> ( spaces >> string "/*"
        >> (many_until any_char_or_nl (string "*/" << spaces) >>$ None) )
    <?> "Cmd" )
      st

  and parse st =
    ( sep_by1 parse_cmd_opt (parse_symb symb_semicolon)
    >>= (fun cmds ->
          let cmds = Option.list_get cmds in
          if Blist.is_empty cmds then zero else return (Blist.map mklc cmds) )
    <?> "CmdList" )
      st

  let _dest_stop = function Stop -> () | _ -> raise WrongCmd

  let _dest_return = function Return -> () | _ -> raise WrongCmd

  let _dest_skip = function Skip -> () | _ -> raise WrongCmd

  let _dest_assign = function Assign (x, e) -> (x, e) | _ -> raise WrongCmd

  let _dest_load = function Load (x, e, s) -> (x, e, s) | _ -> raise WrongCmd

  let _dest_store = function
    | Store (e1, s, e2) -> (e1, s, e2)
    | _ -> raise WrongCmd

  let _dest_new = function New x -> x | _ -> raise WrongCmd

  let _dest_free = function Free e -> e | _ -> raise WrongCmd

  let _dest_if = function If (cond, cmd) -> (cond, cmd) | _ -> raise WrongCmd

  let _dest_ifelse = function
    | IfElse (cond, cmd, cmd') -> (cond, cmd, cmd')
    | _ -> raise WrongCmd

  let _dest_while = function
    | While (cond, cmd) -> (cond, cmd)
    | _ -> raise WrongCmd


  let _dest_parallel = function
  | Parallel (cmd, cmd') -> (cmd, cmd')
  | _ -> raise WrongCmd


  let _dest_proc_call = function
    | ProcCall (p, args) -> (p, args)
    | _ -> raise WrongCmd

  let _dest_deref = function
    | Load (x, e, s) -> e
    | Store (e1, s, e2) -> e1
    | Free e -> e
    | _ -> raise WrongCmd

  let _dest_branching = function
    | While (cond, _) | If (cond, _) | IfElse (cond, _, _) -> cond
    | _ -> raise WrongCmd

  let _dest_assert = function Assert f -> f | _ -> raise WrongCmd

  let dest_cmd f c = f (get_cmd c)

  let dest_stop = dest_cmd _dest_stop

  let dest_return = dest_cmd _dest_return

  let dest_skip = dest_cmd _dest_skip

  let dest_assign = dest_cmd _dest_assign

  let dest_load = dest_cmd _dest_load

  let dest_store = dest_cmd _dest_store

  let dest_new = dest_cmd _dest_new

  let dest_free = dest_cmd _dest_free

  let dest_deref = dest_cmd _dest_deref

  let dest_if = dest_cmd _dest_if

  let dest_ifelse = dest_cmd _dest_ifelse

  let dest_while = dest_cmd _dest_while

  let dest_parallel = dest_cmd _dest_parallel

  let dest_branching = dest_cmd _dest_branching

  let dest_proc_call = dest_cmd _dest_proc_call

  let dest_assert = dest_cmd _dest_assert

  let dest_empty c = if Blist.is_empty c then () else raise WrongCmd

  let number c =
    let rec aux n = function
      | [] -> ([], n)
      | c :: l -> (
        match c.cmd with
        | ProcCall (_, _)
         |Assign _ | Load _ | Store _ | New _ | Free _ | Stop | Return | Skip
          ->
            let c' = {label= Some n; cmd= c.cmd} in
            let l', n' = aux (n + 1) l in
            (c' :: l', n')
        | Assert _ ->
            let l', n' = aux n l in
            (c :: l', n')
        | If (cond, subc) ->
            let subc', n' = aux (n + 1) subc in
            let c' = {label= Some n; cmd= If (cond, subc')} in
            let l', n'' = aux n' l in
            (c' :: l', n'')
        | IfElse (cond, subc1, subc2) ->
            let subc1', n' = aux (n + 1) subc1 in
            let subc2', n'' = aux (n' + 1) subc2 in
            let c' = {label= Some n; cmd= IfElse (cond, subc1', subc2')} in
            let l', n'' = aux n'' l in
            (c' :: l', n'')
        | Parallel (subc1, subc2) ->
              let subc1', n' = aux (n + 1) subc1 in
              let subc2', n'' = aux (n' + 1) subc2 in
              let c' = {label= Some n; cmd= Parallel (subc1', subc2')} in
              let l', n'' = aux n'' l in
              (c' :: l', n'')
        | While (cond, subc) ->
            let subc', n' = aux (n + 1) subc in
            let c' = {label= Some n; cmd= While (cond, subc')} in
            let l', n'' = aux n' l in
            (c' :: l', n'') )
    in
    fst (aux 0 c)

  let rec cmd_terms = function
    | Stop | Return | Skip | Assert _ -> Term.Set.empty
    | New x | Free x -> Term.Set.singleton x
    | Assign (x, e) | Load (x, e, _) | Store (x, _, e) ->
        Term.Set.of_list [x; e]
    | If (cond, cmd) -> Term.Set.union (Cond.vars cond) (terms cmd)
    | IfElse (cond, cmd, cmd') ->
        Term.Set.union
          (Term.Set.union (Cond.vars cond) (terms cmd))
          (terms cmd')
    | While (cond, cmd) -> Term.Set.union (Cond.vars cond) (terms cmd)
    | ProcCall (p, args) -> Term.Set.of_list args
    | Parallel (cmd, cmd') -> Term.Set.union (terms cmd) (terms cmd')

  and terms l =
    Blist.fold_left
      (fun s c -> Term.Set.union s (cmd_terms c.cmd))
      Term.Set.empty l

  let vars cmd = Term.filter_vars (terms cmd)

  let locals params cmd = Term.Set.diff (vars cmd) params

  let rec cmd_modifies ?(strict = true) cmd =
    match cmd with
    | Stop | Return | Skip | Free _ | Assert _ | ProcCall (_, _) ->
        Term.Set.empty
    | New x | Assign (x, _) | Load (x, _, _) -> Term.Set.singleton x
    | Store (x, _, _) ->
        if strict then Term.Set.singleton x else Term.Set.empty
    | If (_, cmd) | While (_, cmd) -> modifies ~strict cmd
    | IfElse (_, cmd, cmd') ->
        Term.Set.union (modifies ~strict cmd) (modifies ~strict cmd')
    | Parallel (cmd, cmd') ->
      Term.Set.union (modifies ~strict cmd) (modifies ~strict cmd')
      

  and modifies ?(strict = true) l =
    Blist.fold_left
      (fun s c -> Term.Set.union s (cmd_modifies ~strict c.cmd))
      Term.Set.empty l

  let rec cmd_equal cmd cmd' =
    match (cmd, cmd') with
    | Stop, Stop | Return, Return | Skip, Skip -> true
    | New x, New y | Free x, Free y -> Term.equal x y
    | Assign (x, e), Assign (x', e') ->
        Term.equal x x' && Term.equal e e'
    | Load (x, e, f), Load (x', e', f') | Store (x, f, e), Store (x', f', e')
      ->
        Term.equal x x' && Term.equal e e' && Field.equal f f'
    | ProcCall (p, args), ProcCall (p', args') ->
        String.equal p p' && Blist.equal Term.equal args args'
    | While (cond, cmd), While (cond', cmd') | If (cond, cmd), If (cond', cmd')
      ->
        Cond.equal cond cond' && equal cmd cmd'
    | IfElse (cond, cmd1, cmd2), IfElse (cond', cmd1', cmd2') ->
        Cond.equal cond cond' && equal cmd1 cmd1' && equal cmd2 cmd2'
    | Parallel (cmd1, cmd2), Parallel (cmd1', cmd2') ->
        equal cmd1 cmd1' && equal cmd2 cmd2'
    | Assert _, _ | _, Assert _ -> raise WrongCmd
    | _ -> false

  and equal l l' =
    match (l, l') with
    | [], [] -> true
    | [], c :: tl -> (
      match c.cmd with Assert _ -> equal [] tl | _ -> false )
    | c :: tl, [] -> (
      match c.cmd with Assert _ -> equal tl [] | _ -> false )
    | c :: tl, c' :: tl' -> (
      match (c.cmd, c'.cmd) with
      | Assert _, _ -> equal tl l'
      | _, Assert _ -> equal l tl'
      | _ -> cmd_equal c.cmd c'.cmd && equal tl tl' )

  let rec subst_cmd theta cmd =
    match cmd with
    | Stop | Return | Skip | Assert _ -> cmd
    | New x -> New (Subst.apply theta x)
    | Free x -> Free (Subst.apply theta x)
    | Assign (x, e) -> Assign (Subst.apply theta x, Subst.apply theta e)
    | Load (x, e, f) -> Load (Subst.apply theta x, Subst.apply theta e, f)
    | Store (x, f, e) ->
        Store (Subst.apply theta x, f, Subst.apply theta e)
    | ProcCall (p, args) -> ProcCall (p, Term.FList.subst theta args)
    | If (cond, cmd) -> If (Cond.subst theta cond, subst theta cmd)
    | IfElse (cond, cmd, cmd') ->
        IfElse (Cond.subst theta cond, subst theta cmd, subst theta cmd')
    | While (cond, cmd) -> While (Cond.subst theta cond, subst theta cmd)
    | Parallel(cmd, cmd') -> Parallel(subst theta cmd, subst theta cmd') (*IMPORTANT: CHECK THIS AGAIN*)
 

  and subst theta cmd =
    match cmd with
    | [] -> []
    | c :: tl -> {c with cmd= subst_cmd theta c.cmd} :: subst theta tl

  let number_width = ref 3

  let indent_by = ref 2

  let pp_label ?(abbr = false) indent fmt c =
    let label =
      match (c.label, abbr) with
      | None, false -> String.make (!number_width + 2) ' '
      | None, true -> ""
      | Some n, false -> Printf.sprintf "%*d: " !number_width n
      | Some n, true -> Printf.sprintf "%d: " n
    in
    let extra_indent = if abbr then "" else String.make indent ' ' in
    Format.pp_print_string fmt (label ^ extra_indent)

  let rec pp_cmd ?(abbr = false) indent fmt c =
    match c.cmd with
    | Stop -> Format.fprintf fmt "%s" keyw_stop.str
    | Return -> Format.fprintf fmt "%s" keyw_return.str
    | Skip -> Format.fprintf fmt "%s" keyw_skip.str
    | Assert f ->
        if abbr then
          Format.fprintf fmt "%s%s...%s" keyw_assert.str symb_lp.str
            symb_rp.str
        else
          Format.fprintf fmt "%s%s%a%s" keyw_assert.str symb_lp.str Form.pp
            f symb_rp.str
    | New x ->
        Format.fprintf fmt "%a%s%s%s%s" Term.pp x symb_assign.sep
          keyw_new.str symb_lp.str symb_rp.str
    | Free x ->
        Format.fprintf fmt "%s%s%a%s" keyw_free.str symb_lp.str Term.pp x
          symb_rp.str
    | Assign (x, e) ->
        Format.fprintf fmt "%a%s%a" Term.pp x symb_assign.sep Term.pp e
    | Load (x, e, f) ->
        Format.fprintf fmt "%a%s%a%s%s" Term.pp x symb_assign.sep Term.pp
          e symb_fld_sel.str f
    | Store (x, f, e) ->
        Format.fprintf fmt "%a%s%s%s%a" Term.pp x symb_fld_sel.str f
          symb_assign.sep Term.pp e
    | ProcCall (p, args) ->
        Format.fprintf fmt "%s(%s)" p
          (Term.FList.to_string_sep symb_comma.sep args)
    | If (cond, cmd) ->
        if abbr then
          Format.fprintf fmt "%s %a %s %a... %s" keyw_if.str Cond.pp cond
            keyw_then.str (pp_label ~abbr 0) (Blist.hd cmd) keyw_fi.str
        else
          Format.fprintf fmt "%s %a %s@\n%a@\n%s" keyw_if.str Cond.pp cond
            keyw_then.str
            (pp ~abbr (indent + !indent_by))
            cmd
            (String.make (!number_width + indent + 2) ' ' ^ keyw_fi.str)
    | IfElse (cond, cmd, cmd') ->
        if abbr then
          Format.fprintf fmt "%s %a %s %a... %s %a... %s" keyw_if.str Cond.pp
            cond keyw_then.str (pp_label ~abbr 0) (Blist.hd cmd) keyw_else.str
            (pp_label ~abbr 0) (Blist.hd cmd') keyw_fi.str
        else
          Format.fprintf fmt "%s %a %s@\n%a@\n%s@\n%a@\n%s" keyw_if.str Cond.pp
            cond keyw_then.str
            (pp ~abbr (indent + !indent_by))
            cmd keyw_else.str
            (pp ~abbr (indent + !indent_by))
            cmd'
            (String.make (!number_width + indent + 2) ' ' ^ keyw_fi.str)
    | While (cond, cmd) ->
        if abbr then
          Format.fprintf fmt "%s %a %s %a... %s" keyw_while.str Cond.pp cond
            keyw_do.str (pp_label ~abbr 0) (Blist.hd cmd) keyw_od.str
        else
          Format.fprintf fmt "%s %a %s@\n%a@\n%s" keyw_while.str Cond.pp cond
            keyw_do.str
            (pp ~abbr (indent + !indent_by))
            cmd
            (String.make (!number_width + indent + 2) ' ' ^ keyw_od.str)
    | Parallel(cmd, cmd') -> 
        Format.fprintf fmt "%s" symb_parallel.str  (* IMPORTANT! MODIFY THIS*)



  and pp_lcmd ?(abbr = false) indent fmt c =
    Format.fprintf fmt "%a%a" (pp_label ~abbr indent) c (pp_cmd ~abbr indent) c

  and pp ?(abbr = false) indent fmt = function
    | [] -> ()
    | [c] -> pp_lcmd ~abbr indent fmt c
    | hd :: tl ->
        if abbr then
          Format.fprintf fmt "%a%s %a..." (pp_lcmd ~abbr indent) hd
            symb_semicolon.str (pp_label ~abbr indent) (Blist.hd tl)
        else
          Format.fprintf fmt "%a%s@\n%a" (pp_lcmd ~abbr indent) hd
            symb_semicolon.str (pp ~abbr indent) tl

  let to_string cmd = mk_to_string (pp ~abbr:true 0) cmd

  let rec strip_asserts = function
    | [] -> []
    | c :: cont -> (
      match c.cmd with Assert _ -> strip_asserts cont | _ -> c :: cont )

  let rec is_while_prog = function
    | [] -> true
    | c :: cs -> (
      match c.cmd with
      | ProcCall _ | Assert _ -> false
      | If (_, c) | While (_, c) -> is_while_prog c && is_while_prog cs
      | IfElse (_, c, c') ->
          is_while_prog c && is_while_prog c' && is_while_prog cs
      | _ -> is_while_prog cs )

  let rec get_dependencies = function
    | [] -> Strng.Set.empty
    | c :: cs -> (
        let rest = get_dependencies cs in
        match c.cmd with
        | ProcCall (p, _) -> Strng.Set.add p rest
        | If (_, c) | While (_, c) -> Strng.Set.union rest (get_dependencies c)
        | IfElse (_, c, c') ->
            Strng.Set.union_of_list
              [rest; get_dependencies c; get_dependencies c']
        | _ -> rest )
end

 
let main = "main"

module Proc = struct
  (** The BasicType kernel (N.B. Procs are equal if they have the same name) *)
  module K = struct
    (** The type of procedures: a tuple of
              - Procedure name
              - Formal parameters
              - A list of pre/post specifications
              - The body of the procedure
        *)
    type t = string * Term.t Blist.t * (Form.t * Form.t) list * Cmd.t

    let compare (id, _, _, _) (id', _, _, _) = Strng.compare id id'

    let equal (id, _, _, _) (id', _, _, _) = Strng.equal id id'

    let hash (id, _, _, _) = Strng.hash id

    let pp_decl fmt (id, params, _, _) =
      Format.fprintf fmt "%s(%a)" id (Blist.pp pp_commasp Term.pp) params

    let pp_specs fmt (_, _, specs, _) =
      let pp_spec fmt (pre, post) =
        Format.fprintf fmt "%s: %a@\n%s: %a" keyw_precondition.str Form.pp
          pre keyw_postcondition.str Form.pp post
      in
      Blist.pp Format.pp_print_newline pp_spec fmt specs

    let pp_head fmt ((id, params, specs, _) as proc) =
      if Blist.is_empty specs then pp_decl fmt proc
      else Format.fprintf fmt "%a@\n@[<1>%a@]" pp_decl proc pp_specs proc

    let pp fmt ((_, _, _, body) as proc) =
      Format.fprintf fmt "%a@\n{@\n@[<2>%a@]@\n}" pp_head proc
        (Cmd.pp ~abbr:true 0) body

    let to_string = mk_to_string pp
  end

  include K
  module Set = Treeset.Make (K)

  (* module Map = Treemap.Make(K) *)
  module SigMap =
    Treemap.Make
      (Pair.Make
         (Pair.Make (Strng) (Term.FList)) (Pair.Make (Form) (Form)))
  module Graph = Graph.Imperative.Digraph.ConcreteBidirectional (K)

  let get_name ((id, _, _, _) : t) = id

  let get_params ((_, params, _, _) : t) = params

  let get_spec_list ((_, _, specs, _) : t) = specs

  let get_body ((_, _, _, body) : t) = body

  let get_seqs ((id, params, specs, _) : t) =
    let cmd = Cmd.mk_proc_call id params in
    Blist.map (fun (pre, post) -> (pre, cmd, post)) specs

  let number_cmds ((id, params, specs, body) : t) =
    (id, params, specs, Cmd.number body)

  let get_dependencies ((id, _, _, body) : t) = Cmd.get_dependencies body

  (* precondition: PRECONDITION; COLON; f = formula; SEMICOLON { f } *)
  let parse_precondition st =
    While.Program.parse_precondition ~allow_tags:true st

  (* postcondition: POSTCONDITION; COLON; f = formula; SEMICOLON { f } *)
  let parse_postcondition st =
    ( parse_symb keyw_postcondition
    >> parse_symb symb_colon >> Form.parse
    >>= (fun f -> parse_symb symb_semicolon >>$ f)
    <?> "Postcondition" )
      st

  let ensure_tags (pre, post) =
    let tags = Tags.union (Form.tags pre) (Form.tags post) in
    let pre' = Form.complete_tags tags pre in
    let inst_subst =
      Tagpairs.mk_free_subst tags (Tags.diff (Form.tags pre') tags)
    in
    let pre' = Form.subst_tags inst_subst pre' in
    let post' = Form.complete_tags tags post in
    (pre', post')

  let check_spec (params, body) (pre, post) =
    (* - parameters mentioned in the postcondition are not assigned to  *)
    (*   in the procedure body;                                         *)
    assert (
      Term.Set.is_empty
        (Term.Set.inter (Form.vars post)
           (Term.Set.inter
              (Cmd.modifies ~strict:false body)
              (Term.Set.of_list params))) ) ;
    (* - local variables are not mentioned in the pre/post;             *)
    assert (
      Term.Set.is_empty
        (Term.Set.inter (Form.vars pre)
           (Cmd.locals (Term.Set.of_list params) body)) ) ;
    assert (
      Term.Set.is_empty
        (Term.Set.inter (Form.vars post)
           (Cmd.locals (Term.Set.of_list params) body)) ) ;
    ()

  let parse_named st =
    let parse_params st =
      let rec parse_params' acc st =
        let tail st =
          let try_parse_next_param check msg st =
            ( look_ahead
                (Term.parse >>= fun p -> if check p then zero else return ())
            <|> fail msg )
              st
          in
          ( followed_by Term.parse ""
          >> try_parse_next_param
               (fun p -> Term.is_nil p)
               "Not a formal parameter"
          >> try_parse_next_param
               (fun p -> Term.is_exist_var p)
               "Not a formal parameter - must not be primed (')"
          >> try_parse_next_param
               (fun p -> List.mem p acc)
               "Duplicate parameter"
          >> Term.parse
          >>= fun p -> parse_params' (p :: acc) )
            st
        in
        ( (if List.length acc == 0 then tail else parse_symb symb_comma >> tail)
        <|> return (Blist.rev acc) )
          st
      in
      parse_params' [] st
    in
    ( parse_symb keyw_proc >> parse_ident
    >>= fun id ->
    Tokens.parens parse_params
    >>= fun params ->
    let spec_parser st =
      ( parse_precondition
      >>= fun pre ->
      parse_postcondition >>= fun post -> return (ensure_tags (pre, post)) )
        st
    in
    many1 spec_parser
    >>= fun specs ->
    Tokens.braces
      (expect_before Cmd.parse (parse_symb symb_rb) "Expecting CmdList")
    >>= fun body ->
    let () = Blist.iter (check_spec (params, body)) specs in
    (* let specs =                                                        *)
    (*   Blist.bind                                                       *)
    (*   (fun (pre, post) ->                                              *)
    (*     Blist.map (Fun.swap Pair.mk post) (Form.all_symheaps pre))  *)
    (*   specs in                                                         *)
    return (id, params, specs, body) <?> "Procedure" )
      st

  let parse_unnamed st =
    ( parse_precondition
    >>= (fun pre ->
          parse_postcondition
          >>= fun post ->
          (* let f v = Term.is_exist_var v in *)
          (* assert(Term.Set.is_empty (Term.Set.inter (Term.Set.filter f (Form.vars pre)) (Term.Set.filter f (Form.vars post)))); *)
          Cmd.parse
          >>= fun body ->
          let pre, post = ensure_tags (pre, post) in
          return (pre, body, post) )
    <?> "CmdList" )
      st
end

module Seq = struct
  type t = Form.t * Cmd.t * Form.t

  let tagset_one = Tags.singleton Tags.anonymous

  let tagpairs_one = Tagpairs.mk tagset_one

  let form_tags f = if !termination then Form.tags f else tagset_one

  let tags (pre, _, _) = form_tags pre

  let all_tags (pre, _, post) =
    Tags.union (Form.tags pre) (Form.tags post)

  let tag_pairs (pre, _, _) = Tagpairs.mk (form_tags pre)

  let vars (pre, _, post) =
    Term.Set.union (Form.vars pre) (Form.vars post)

  let all_vars (pre, cmd, post) =
    Term.Set.union_of_list
      [Form.vars pre; Cmd.vars cmd; Form.vars post]

  let terms (pre, _, post) =
    Term.Set.union (Form.terms pre) (Form.terms post)

  let subst theta (pre, cmd, post) =
    (Form.subst theta pre, cmd, Form.subst theta post)

  let subst_tags tps (pre, cmd, post) =
    (Form.subst_tags tps pre, cmd, Form.subst_tags tps post)

  let param_subst theta (pre, cmd, post) =
    (Form.subst theta pre, Cmd.subst theta cmd, Form.subst theta post)

  let with_pre (_, cmd, post) pre = (pre, cmd, post)

  let with_post (pre, cmd, _) post = (pre, cmd, post)

  let with_cmd (pre, _, post) cmd = (pre, cmd, post)

  let to_string (pre, cmd, post) =
    symb_turnstile.sep ^ symb_lb.str ^ Form.to_string pre ^ symb_rb.str
    ^ " " ^ Cmd.to_string cmd ^ symb_lb.str ^ Form.to_string post
    ^ symb_rb.str

  let subsumed (pre, cmd, post) (pre', cmd', post') =
    Cmd.equal cmd cmd' && Form.subsumed pre' pre
    && Form.subsumed post post'

  let subsumed_upto_tags (pre, cmd, post) (pre', cmd', post') =
    Cmd.equal cmd cmd'
    && Form.subsumed_upto_tags pre' pre
    && Form.subsumed_upto_tags post post'

  let pp fmt (pre, cmd, post) =
    Format.fprintf fmt "@[%s{%a}@ %a@ {%a}@]" symb_turnstile.sep Form.pp pre
      (Cmd.pp ~abbr:true 0) cmd Form.pp post

  (* Tags.pp (tags seq) *)

  let equal (pre, cmd, post) (pre', cmd', post') =
    Cmd.equal cmd cmd' && Form.equal pre pre' && Form.equal post post'

  let equal_upto_tags (pre, cmd, post) (pre', cmd', post') =
    Cmd.equal cmd cmd'
    && Form.equal_upto_tags pre pre'
    && Form.equal_upto_tags post post'

  let dest (pre, cmd, post) = (Form.dest pre, cmd, Form.dest post)

  let get_tracepairs (pre, _, _) (pre', _, _) =
    let tps = Form.get_tracepairs pre pre' in
    Pair.map (Tagpairs.filter (fun (t, _) -> Tags.is_free_var t)) tps

  let frame f (pre, cmd, post) =
    ( Form.star ~augment_deqs:false pre f
    , cmd
    , Form.star ~augment_deqs:false post f )
end

let pp_prog fmt (fields, procs) =
  Format.fprintf fmt "%a@\n@\n%a" Field.pp_fields fields
    (Blist.pp pp_dbl_nl Proc.pp)
    procs

let program_vars = ref Term.Set.empty

(* Store a list as well as a map so the procedures can be printed in the order they were provided *)
let proc_list = ref []

let proc_map = ref Strng.Map.empty

module Operations = Graph.Oper.I (Proc.Graph)

let dependencies = ref (Proc.Graph.create ())

let reachability = ref (Proc.Graph.create ())

let pp fmt () = pp_prog fmt (Field.get_fields (), !proc_list)

let get_proc p = Strng.Map.find p !proc_map

let set_program (fields, procs) =
  Field.reset () ;
  Blist.iter Field.add fields ;
  proc_list := procs ;
  program_vars :=
    Blist.foldl
      (fun vars (_, params, _, body) ->
        Term.Set.union_of_list
          [vars; Term.Set.of_list params; Cmd.vars body] )
      Term.Set.empty procs ;
  proc_map :=
    Blist.fold_left
      (fun procs ((id, _, _, _) as p) -> Strng.Map.add id p procs)
      Strng.Map.empty procs ;
  Proc.Graph.clear !dependencies ;
  Blist.iter
    (fun p ->
      Proc.Graph.add_vertex !dependencies p ;
      Strng.Set.iter
        (fun p' -> Proc.Graph.add_edge !dependencies p (get_proc p'))
        (Proc.get_dependencies p) )
    procs ;
  reachability := Operations.transitive_closure ~reflexive:true !dependencies

let get_reachable ps =
  let graph = Proc.Graph.copy !dependencies in
  let () =
    Strng.Map.iter
      (fun id p ->
        if
          not
            (Blist.exists
               (fun p' ->
                 Proc.Graph.mem_edge !reachability
                   (Strng.Map.find p' !proc_map)
                   p )
               ps)
        then Proc.Graph.remove_vertex graph p )
      !proc_map
  in
  graph

let vars_of_program () = !program_vars

(* remember prog vars when introducing fresh ones *)
let fresh_fvar s = Term.fresh_fvar (Term.Set.union !program_vars s)

let fresh_fvars s i = Term.fresh_fvars (Term.Set.union !program_vars s) i

let fresh_evar s = Term.fresh_evar (Term.Set.union !program_vars s)

let fresh_evars s i = Term.fresh_evars (Term.Set.union !program_vars s) i

(* again, treat prog vars as special *)
let freshen_case_by_seq seq case =
  Indrule.freshen (Term.Set.union !program_vars (Seq.vars seq)) case

(* fields: FIELDS; COLON; ils = separated_nonempty_list(COMMA, IDENT); SEMICOLON  *)
(*     { List.iter P.Field.add ils }                                              *)
let parse_fields st =
  ( parse_symb keyw_fields >> parse_symb symb_colon
  >> sep_by1 Field.parse (parse_symb symb_comma)
  <?> "Fields" )
    st

(* procedures *)
let parse_procs st =
  ( many Proc.parse_named
  |>> fun procs ->
  Blist.foldr
    (fun p ps ->
      assert (not (Blist.exists (Proc.equal p) ps)) ;
      p :: ps )
    procs [] )
    st

(* main: p = precondition; q = postcondition; cmd = command; EOF { (p, cmd, q) } *)
let parse_main st = (Proc.parse_unnamed << eof <?> "Main procedure") st

(* fields; procs = procedures; { (main, procs) } *)
let parse st =
  ( parse_fields << parse_symb symb_semicolon
  >>= (fun fields -> parse_procs << eof |>> fun procs -> (fields, procs))
  <?> "Program" )
    st

let of_channel c = handle_reply (parse_channel parse c ())
