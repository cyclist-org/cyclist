open Util
open Lib
open Symbols
open MParser

module SH = Asl_heap

exception WrongCmd

let is_prog_var v = Asl_term.is_free_var v
let is_prog_term t = Asl_term.Set.for_all is_prog_var (Asl_term.filter_vars (Asl_term.Set.singleton t))

module Cond =
  struct
    type t =
      | Eq of Asl_term.t * Asl_term.t
      | Ne of Asl_term.t * Asl_term.t (* != *)
      | Lt of Asl_term.t * Asl_term.t (* < *)
      | Le of Asl_term.t * Asl_term.t (* <= *)
      | Non_det

    let mk_eq e1 e2 = assert(is_prog_term e1); assert(is_prog_term e2); Eq(e1,e2)
    let mk_ne e1 e2 = assert(is_prog_term e1); assert(is_prog_term e2); Ne(e1,e2)
    let mk_lt e1 e2 = assert(is_prog_term e1); assert(is_prog_term e2); Lt(e1,e2)
    let mk_le e1 e2 = assert(is_prog_term e1); assert(is_prog_term e2); Le(e1,e2)
    let mk_non_det () = Non_det

    let is_ne = function
      | Ne(_, _) -> true
      | _ -> false
    let is_eq = function
      | Eq(_, _) -> true
      | _ -> false
    let is_le = function
      | Le(_, _) -> true
      | _ -> false
    let is_lt = function
      | Lt(_, _) -> true
      | _ -> false
    let is_non_det = function
      | Non_det -> true
      | _ -> false
    let is_det c = not (is_non_det c)

    let dest = function
      | Eq(e1, e2) | Ne(e1, e2) | Lt(e1, e2) | Le(e1, e2) -> (e1,e2)
      | Non_det -> raise WrongCmd

    let terms = function
      | Non_det -> Asl_term.Set.empty
      | Ne(x,y) | Eq(x,y) | Lt(x,y) | Le(x,y) -> Asl_term.Set.add x (Asl_term.Set.singleton y)

    let vars cond = Asl_term.filter_vars (terms cond)

    let equal cond cond' = match (cond, cond') with
      | (Non_det, Non_det) -> true
      | (Eq(x,y), Eq(x',y')) | (Ne(x,y), Ne(x',y')) | (Lt(x,y), Lt(x',y')) | (Le(x,y), Le(x',y')) ->
        Asl_term.equal x x' && Asl_term.equal y y'
      | _ -> false

    let subst theta cond = match cond with
      | Eq(x, y) -> Eq ((Asl_term.subst theta x), (Asl_term.subst theta y))
      | Ne(x, y) -> Ne ((Asl_term.subst theta x), (Asl_term.subst theta y))
      | Lt(x, y) -> Lt ((Asl_term.subst theta x), (Asl_term.subst theta y))
      | Le(x, y) -> Le ((Asl_term.subst theta x), (Asl_term.subst theta y))
      | _ -> cond

    let pp fmt = function
      | Non_det ->
        Format.fprintf fmt "@[%s@]" symb_star.str
      | Eq(x,y) ->
        Format.fprintf fmt "@[%a%s%a@]" Asl_term.pp x symb_eq.str Asl_term.pp y
      | Ne(x,y) ->
        Format.fprintf fmt "@[%a%s%a@]" Asl_term.pp x symb_deq.str Asl_term.pp y
      | Le(x,y) ->
        Format.fprintf fmt "@[%a%s%a@]" Asl_term.pp x symb_le.str Asl_term.pp y
      | Lt(x,y) ->
        Format.fprintf fmt "@[%a%s%a@]" Asl_term.pp x symb_lt.str Asl_term.pp y

    let to_melt = function
      | Non_det -> symb_star.melt
      | Eq(x,y) -> Latex.concat [Asl_term.to_melt x; symb_eq.melt; Asl_term.to_melt y]
      | Ne(x,y) -> Latex.concat [Asl_term.to_melt x; symb_deq.melt; Asl_term.to_melt y]
      | Lt(x, y) -> Latex.concat [Asl_term.to_melt x; symb_lt.melt; Asl_term.to_melt y]
      | Le(x, y) -> Latex.concat [Asl_term.to_melt x; symb_le.melt; Asl_term.to_melt y]

    (* FIXME: double check *)
    let fork f c =
      if is_non_det c then (f,f) else
      let (t1, t2) = dest c in
        match c with
        | Eq _ -> (SH.with_eqs f (Asl_uf.add (t1, t2) f.SH.eqs), SH.with_neqs f (Asl_neqs.add (t1, t2) f.SH.neqs))
        | Ne _ -> (SH.with_neqs f (Asl_neqs.add (t1, t2) f.SH.neqs), SH.with_eqs f (Asl_uf.add (t1, t2) f.SH.eqs))
        | Lt _ -> (SH.with_lts f (Asl_lts.add (t1, t2) f.SH.lts), SH.with_leqs f (Asl_leqs.add (t2, t1) f.SH.leqs))
        | Le _ -> (SH.with_leqs f (Asl_leqs.add (t1, t2) f.SH.leqs), SH.with_lts f (Asl_lts.add (t2, t1) f.SH.lts))
        | _ -> raise WrongCmd
      
    let validated_by h = function
      | Eq(x, y) -> Asl_heap.equates h x y
      | Ne(x, y) -> Asl_heap.disequates h x y
      | Lt(x, y) -> Asl_heap.entail_lt h x y
      | Le(x, y) -> Asl_heap.entail_le h x y
      | _ -> false
    
    let invalidated_by h = function
      | Eq(x, y) -> Asl_heap.disequates h x y
      | Ne(x, y) -> Asl_heap.equates h x y
      | Lt(x, y) -> (Asl_heap.entail_lt h y x) || (Asl_heap.entail_le h y x)
      | Le(x, y) -> ((Asl_heap.entail_le h y x) && (Asl_heap.disequates h x y)) || (Asl_heap.entail_lt h y x)
      | _ -> false
    
    let parse st =
      ( attempt (parse_symb symb_star >>$ mk_non_det ()) <|>
        attempt (Asl_uf.parse |>> Fun.uncurry mk_eq) <|>
        attempt (Asl_leqs.parse |>> Fun.uncurry mk_le) <|>
        attempt (Asl_lts.parse |>> Fun.uncurry mk_lt) <|>
                (Asl_neqs.parse |>> Fun.uncurry mk_ne) <?> "Cond") st
  end


module Cmd =
  struct
    type cmd_t =
      | Stop
      | Skip
      | Assign of Asl_term.t * Asl_term.t
      | Load of Asl_term.t * Asl_term.t * Asl_term.t
      | Store of Asl_term.t * Asl_term.t * Asl_term.t
      | New of Asl_term.t * Asl_term.t
      | Free of Asl_term.t * Asl_term.t
      | If of Cond.t * t
      | IfElse of Cond.t * t * t
      | While of Cond.t * t
      (* | ProcCall of string * Sl_term.FList.t *)
    and basic_t = { label:int option; cmd:cmd_t }
    and t = basic_t list

    let get_cmd c = if c=[] then raise WrongCmd else (Blist.hd c).cmd
    let get_cont c = if c=[] then raise WrongCmd else Blist.tl c

    let is_empty c = c=[]
    let is_not_empty c = not (is_empty c)

    let is_assign c = is_not_empty c && match get_cmd c with
      | Assign _ -> true
      | _ -> false
    let is_load c = is_not_empty c && match get_cmd c with
      | Load _ -> true
      | _ -> false
    let is_store c = is_not_empty c && match get_cmd c with
      | Store _ -> true
      | _ -> false
    let is_new c = is_not_empty c && match get_cmd c with
      | New _ -> true
      | _ -> false
    let is_free c = is_not_empty c && match get_cmd c with
      | Free _ -> true
      | _ -> false
    let is_stop c = is_not_empty c && match get_cmd c with
      | Stop -> true
      | _ -> false
    let is_skip c = is_not_empty c && match get_cmd c with
      | Skip -> true
      | _ -> false
    (* let is_proc_call c = is_not_empty c && match get_cmd c with
      | ProcCall(_, _) -> true
      | _ -> false *)
    

    let is_basic c = is_not_empty c && match get_cmd c with
       Assign _  | Load _ | Store _ | New _ | Free _ | Stop | Skip -> true
      | _ -> false

    let is_if c = is_not_empty c && match get_cmd c with
      | If _ -> true
      | _ -> false
    let is_ifelse c = is_not_empty c && match get_cmd c with
      | IfElse _ -> true
      | _ -> false
    let is_while c = is_not_empty c && match get_cmd c with
      | While _ -> true
      | _ -> false

    let mklc c = { label=None; cmd=c }
    let mk_basic c = [ { label=None; cmd=c } ]
    let mk_assign x e = mk_basic (Assign(x,e))
    let mk_load x y e =  mk_basic (Load(x,y,e))
    let mk_store y e x = mk_basic (Store(y,e,x))
    let mk_new e1 e2 = mk_basic (New(e1, e2))
    let mk_free v n = mk_basic (Free(v, n))
    let mk_stop = mk_basic (Stop)
    let mk_skip = mk_basic (Skip)
    (* let mk_proc_call p args = mk_basic (ProcCall(p, args)) *)
    let mk_if cond cmd = mk_basic (If(cond, cmd))
    let mk_ifelse cond cmd cmd' = mk_basic (IfElse(cond, cmd, cmd'))
    let mk_while cond cmd = mk_basic (While(cond, cmd))
    let mk_seq cmd cmd' = cmd @ cmd'
    let mk_from_list l = Blist.flatten l

    (* FIXME: to change as neccessary *)
    let rec parse_cmd st = 
      (   attempt (parse_symb keyw_stop >>$ Stop)
      <|> attempt (parse_symb keyw_skip >>$ Skip)
      <|> attempt (parse_symb keyw_free >>
          parse_symb symb_lp >>
          Asl_term.parse >>= (fun v ->
          parse_symb symb_comma >>
          Asl_term.parse <<
          parse_symb symb_rp |>> (fun n ->
          assert(is_prog_var v); assert(is_prog_term n); Free(v, n))))
      <|> attempt (parse_symb keyw_if >>
          Cond.parse >>= (fun cond ->
          parse_symb keyw_then >>
          parse >>= (fun cmd1 ->
          parse_symb keyw_else >>
          parse >>= (fun cmd2 ->
          parse_symb keyw_fi >>$ IfElse(cond,cmd1,cmd2)))))
      <|> attempt (parse_symb keyw_if >>
          Cond.parse >>= (fun cond ->
          parse_symb keyw_then >>
          parse >>= (fun cmd ->
          parse_symb keyw_fi >>$ If(cond,cmd))))
      <|> attempt (parse_symb keyw_while >>
          Cond.parse >>= (fun cond ->
          parse_symb keyw_do >>
          parse >>= (fun cmd ->
          parse_symb keyw_od >>$ While(cond,cmd))))
      (*   v = var; ASSIGN; NEW; array *)
      <|> attempt (Asl_term.parse >>= (fun v -> 
          parse_symb symb_assign >>
          parse_symb keyw_new >>
          parse_symb symb_array >>
          MParser_PCRE.Tokens.squares Asl_term.parse <<
          spaces |>> (fun n ->
          assert (is_prog_term v) ; New(v, n))))
      (* v = var; ASSIGN; v2 = var ; LS; t = term; RS *)
      <|> attempt ( Asl_term.parse >>= (fun v -> 
          parse_symb symb_assign >>
          Asl_term.parse >>= (fun v2 ->
          MParser_PCRE.Tokens.squares Asl_term.parse <<
          spaces |>> (fun t -> 
          assert (is_prog_var v); assert(is_prog_term t);
          assert (is_prog_var v2); Load(v,v2,t)))))
      (* v = var; LS; t = term; RS; ASSIGN; v2 = var *)
      <|> attempt ( Asl_term.parse >>= (fun v ->
          MParser_PCRE.Tokens.squares Asl_term.parse >>= (fun t -> 
          parse_symb symb_assign >>
          Asl_term.parse <<
          spaces |>> (fun v2 ->
          assert (is_prog_var v); assert(is_prog_term t);
          assert (is_prog_term v2); Store(v,t,v2)))))
      (* | v = var; ASSIGN; t = term { P.Cmd.mk_assign v t } *)
      <|> ( Asl_term.parse >>= (fun v -> 
          parse_symb symb_assign >> 
          Asl_term.parse |>> (fun t -> 
          assert (is_prog_var v) ; assert(is_prog_term t); Assign(v,t))) )
      (*<|> (parse_ident >>= (fun p ->
          Tokens.parens (Tokens.comma_sep Sl_term.parse) |>> (fun args ->
          (Blist.iter (fun arg -> assert(is_prog_var arg || Sl_term.is_nil arg)) args);
          ProcCall(p, args))))*)
      <?> "Cmd") st
    and parse st = 
      (sep_by1 parse_cmd (parse_symb symb_semicolon) |>> 
      Blist.map mklc <?> "CmdList") st

    let _dest_stop = function
      | Stop -> ()
      | _ -> raise WrongCmd
    let _dest_skip = function
      | Skip -> ()
      | _ -> raise WrongCmd
    let _dest_assign = function
      | Assign(x,e) -> (x,e)
      | _ -> raise WrongCmd
    let _dest_load = function
      | Load(x,y,e) -> (x,y,e)
      | _ -> raise WrongCmd
    let _dest_store = function
      | Store(y,e,x) -> (y,e,x)
      | _ -> raise WrongCmd
    let _dest_new = function
      | New(e1, e2) -> (e1, e2)
      | _ -> raise WrongCmd
    let _dest_free = function
      | Free(v, n) -> (v, n)
      | _ -> raise WrongCmd
    let _dest_if = function
      | If(cond,cmd) -> (cond,cmd)
      | _ -> raise WrongCmd
    let _dest_ifelse = function
      | IfElse(cond,cmd,cmd') -> (cond,cmd,cmd')
      | _ -> raise WrongCmd
    let _dest_while = function
      | While(cond,cmd) -> (cond,cmd)
      | _ -> raise WrongCmd
    (* let _dest_proc_call = function
      | ProcCall(p, args) -> (p, args)
      | _ -> raise WrongCmd *)
    let _dest_branching = function
      | While(cond,_) | If(cond,_) | IfElse(cond,_,_) -> cond
      | _ -> raise WrongCmd 

    let dest_cmd f = fun c -> f (get_cmd c)

    let dest_stop = dest_cmd _dest_stop
    let dest_skip = dest_cmd _dest_skip
    let dest_assign = dest_cmd _dest_assign
    let dest_load = dest_cmd _dest_load
    let dest_store = dest_cmd _dest_store
    let dest_new = dest_cmd _dest_new
    let dest_free = dest_cmd _dest_free
    let dest_if = dest_cmd _dest_if
    let dest_ifelse = dest_cmd _dest_ifelse
    let dest_while = dest_cmd _dest_while
    let dest_branching = dest_cmd _dest_branching
    (* let dest_proc_call = dest_cmd _dest_proc_call *)
    let dest_empty c = if c=[] then () else raise WrongCmd

    let number c =
      let rec aux n = function
        | [] -> ([], n)
        | c::l ->
          begin match c.cmd with
            | Assign _ | Load _ | Store _ | New _ | Free _ | Stop | Skip ->
              let c' = { label=Some n; cmd=c.cmd } in
              let (l', n') = aux (n+1) l in
              (c'::l', n')
            | If(cond, subc) ->
              let (subc', n') = aux (n+1) subc in
              let c' = { label=Some n; cmd=If(cond, subc') } in
              let (l', n'') = aux n' l in
              (c'::l', n'')
            | IfElse(cond, subc1,subc2) ->
              let (subc1', n') = aux (n+1) subc1 in
              let (subc2', n'') = aux (n'+1) subc2 in
              let c' = { label=Some n; cmd=IfElse(cond, subc1',subc2') } in
              let (l', n'') = aux n'' l in
              (c'::l', n'')
            | While(cond, subc) ->
              let (subc', n') = aux (n+1) subc in
              let c' = { label=Some n; cmd=While(cond, subc') } in
              let (l', n'') = aux n' l in
              (c'::l', n'')
          end in
      fst (aux 0 c)

    let rec cmd_terms = function
      | Stop | Skip -> Asl_term.Set.empty
      | New(e1, e2) -> Asl_term.Set.of_list [e1; e2]
      | Free(v, n) -> Asl_term.Set.of_list [v; n]
      | Assign(x,e) -> Asl_term.Set.of_list [x; e]
      | Load(x,y,e) | Store(y,e,x) -> Asl_term.Set.of_list [x; y; e]
      | If(cond,cmd) -> Asl_term.Set.union (Cond.vars cond) (terms cmd)
      | IfElse(cond,cmd,cmd') ->
        Asl_term.Set.union (Asl_term.Set.union (Cond.vars cond) (terms cmd)) (terms cmd')
      | While(cond,cmd) -> Asl_term.Set.union (Cond.vars cond) (terms cmd)
      (* | ProcCall(p, args) -> Sl_term.Set.of_list args *)
    and terms l =
      Blist.fold_left (fun s c -> Asl_term.Set.union s (cmd_terms c.cmd)) Asl_term.Set.empty l

    let vars cmd = Asl_term.filter_vars (terms cmd)
    
    let locals params cmd = Asl_term.Set.diff (vars cmd) params

    (* FIXME: remove flag??? *)
    let rec cmd_modifies ?strict:(flag=true) cmd = match cmd with
      | Stop | Skip | Free _ | Store _ -> Asl_term.Set.empty
      | New (x, _) | Assign(x,_) | Load(x,_,_) -> Asl_term.Set.singleton x
      | If(_,cmd) | While(_,cmd) -> modifies ~strict:flag cmd
      | IfElse(_,cmd,cmd') -> 
          Asl_term.Set.union (modifies ~strict:flag cmd) (modifies ~strict:flag cmd')
    and modifies ?strict:(flag=true) l =
      Blist.fold_left 
        (fun s c -> Asl_term.Set.union s (cmd_modifies ~strict:flag c.cmd)) Asl_term.Set.empty l
        
    let rec cmd_equal cmd cmd' = match (cmd, cmd') with
      | (Stop, Stop) | (Skip, Skip) -> true
      | (New(x1, x2), New(y1, y2)) -> (Asl_term.equal x1 y1) && (Asl_term.equal x2 y2)
      | (Assign(x,e), Assign(x',e')) -> Asl_term.equal x x' && Asl_term.equal e e'
      | (Free(v1, n1), Free(v2, n2)) -> Asl_term.equal v1 v2 && Asl_term.equal n1 n2
      | (Load(x,y,e), Load(x',y',e')) | (Store(y,e,x), Store(y',e',x')) ->
        Asl_term.equal x x' && Asl_term.equal e e' && Asl_term.equal y y'
      (* | (ProcCall(p, args), ProcCall(p', args')) -> 
        p=p' && Blist.equal Sl_term.equal args args' *)
      | (While(cond,cmd), While(cond',cmd')) | (If(cond,cmd), If(cond',cmd')) ->
        Cond.equal cond cond' && equal cmd cmd'
      | (IfElse(cond,cmd1,cmd2), IfElse(cond',cmd1',cmd2')) ->
        Cond.equal cond cond' && equal cmd1 cmd1' && equal cmd2 cmd2'
      | _ -> false
    and equal l l' = match (l,l') with
      | ([], []) -> true
      | ([], _) | (_, []) -> false
      | (c::tl, c'::tl') -> cmd_equal c.cmd c'.cmd && equal tl tl'

    let rec subst_cmd theta cmd = match cmd with
      | Stop | Skip -> cmd
      | New(e1, e2) -> New (Asl_term.subst theta e1, Asl_term.subst theta e2)
      | Free(v, n) -> Free (Asl_term.subst theta v, Asl_term.subst theta n)
      | Assign(x, e) -> Assign ((Asl_term.subst theta x), (Asl_term.subst theta e))
      | Load(x, y, e) -> Load ((Asl_term.subst theta x), (Asl_term.subst theta y), (Asl_term.subst theta e))
      | Store(y, e, x) -> Store ((Asl_term.subst theta y), (Asl_term.subst theta e), (Asl_term.subst theta x))
      (* | ProcCall(p, args) -> ProcCall (p, (Sl_term.FList.subst theta args)) *)
      | If(cond, cmd) -> If ((Cond.subst theta cond), (subst theta cmd))
      | IfElse(cond, cmd, cmd') -> IfElse ((Cond.subst theta cond), (subst theta cmd), (subst theta cmd'))
      | While(cond, cmd) -> While ((Cond.subst theta cond), (subst theta cmd))
    and subst theta cmd = match cmd with
      | [] -> []
      | c::tl -> {c with cmd = (subst_cmd theta c.cmd)} :: (subst theta tl) 

    let number_width = ref 3
    let indent_by = ref 2

    let pp_label ?(abbr=false) indent fmt c =
      let label = match (c.label, abbr) with
        | (None, false) -> String.make (!number_width+2) ' '
        | (None, true) -> ""
        | (Some n, false) -> Printf.sprintf "%*d: " !number_width n
        | (Some n, true) -> Printf.sprintf "%d: " n in
      let extra_indent = if abbr then "" else String.make indent ' ' in
      Format.pp_print_string fmt (label ^ extra_indent)

    let rec pp_cmd ?(abbr=false) indent fmt c = match c.cmd with
      | Stop -> Format.fprintf fmt "%s" keyw_stop.str
      | Skip -> Format.fprintf fmt "%s" keyw_skip.str
      | New(t1, t2) ->
        Format.fprintf fmt "%a%s%s%s%s%a%s"
          Asl_term.pp t1 symb_assign.sep keyw_new.sep symb_array.str symb_ls.str Asl_term.pp t2 symb_rs.str
      | Free(v, n ) ->
        Format.fprintf fmt "%s%s%a%s%a%s"
          keyw_free.str symb_lp.str Asl_term.pp v symb_comma.sep Asl_term.pp n symb_rp.str
      | Assign(x,e) ->
        Format.fprintf fmt "%a%s%a"
          Asl_term.pp x symb_assign.sep Asl_term.pp e
      | Load(x,y,e) ->
        Format.fprintf fmt "%a%s%a%s%a%s"
          Asl_term.pp x symb_assign.sep Asl_term.pp y symb_ls.str Asl_term.pp e symb_rs.str
      | Store(y,e,x) ->
        Format.fprintf fmt "%a%s%a%s%s%a"
          Asl_term.pp y symb_ls.str Asl_term.pp e symb_rs.str symb_assign.sep Asl_term.pp x
      (*| ProcCall(p, args) -> Format.fprintf fmt "%s(%s)"
          p (Sl_term.FList.to_string_sep symb_comma.sep args) *)
      | If(cond,cmd) ->
        if abbr then
          Format.fprintf fmt "%s %a %s %a... %s"
            keyw_if.str Cond.pp cond keyw_then.str (pp_label ~abbr 0) (Blist.hd cmd) keyw_fi.str
        else
          Format.fprintf fmt "%s %a %s@\n%a@\n%s"
            keyw_if.str Cond.pp cond keyw_then.str (pp ~abbr (indent+ !indent_by)) cmd
              ((String.make (!number_width+indent+2) ' ') ^ keyw_fi.str)
      | IfElse(cond,cmd,cmd') ->
        if abbr then
          Format.fprintf fmt "%s %a %s %a... %s %a... %s"
            keyw_if.str Cond.pp cond keyw_then.str (pp_label ~abbr 0) (Blist.hd cmd)
            keyw_else.str (pp_label ~abbr 0) (Blist.hd cmd') keyw_fi.str
        else
          Format.fprintf fmt "%s %a %s@\n%a@\n%s@\n%a@\n%s"
            keyw_if.str Cond.pp cond keyw_then.str (pp ~abbr (indent+ !indent_by)) cmd
            keyw_else.str (pp ~abbr (indent+ !indent_by)) cmd'
              ((String.make (!number_width+indent+2) ' ') ^ keyw_fi.str)
      | While(cond,cmd) ->
        if abbr then
          Format.fprintf fmt "%s %a %s %a... %s"
            keyw_while.str Cond.pp cond keyw_do.str
            (pp_label ~abbr 0) (Blist.hd cmd) keyw_od.str
        else
          Format.fprintf fmt "%s %a %s@\n%a@\n%s"
            keyw_while.str Cond.pp cond keyw_do.str
            (pp ~abbr (indent+ !indent_by)) cmd
            ((String.make (!number_width+indent+2) ' ') ^ keyw_od.str)
    and pp_lcmd ?(abbr=false) indent fmt c =
      Format.fprintf fmt "%a%a"
        (pp_label ~abbr indent) c (pp_cmd ~abbr indent) c
    and pp ?(abbr=false) indent fmt = function
      | [] -> ()
      | [ c ] -> pp_lcmd ~abbr indent fmt c
      | hd::tl ->
        if abbr then
          Format.fprintf fmt "%a%s %a..."
            (pp_lcmd ~abbr indent) hd symb_semicolon.str
            (pp_label ~abbr indent) (Blist.hd tl)
        else
          Format.fprintf fmt "%a%s@\n%a"
            (pp_lcmd ~abbr indent) hd symb_semicolon.str (pp ~abbr indent) tl

    let to_string cmd = mk_to_string (pp ~abbr:true 0) cmd
     
    let to_melt_label c = match c.label with
        | None -> Latex.empty
        | Some n -> Latex.text ((string_of_int n) ^ " : ")

    let rec to_melt_cmd c = match c.cmd with
      | Stop -> keyw_stop.melt
      | Skip -> keyw_skip.melt
      | New(t1, t2) ->
        Latex.concat
          [ Asl_term.to_melt t1; symb_assign.melt; keyw_new.melt; symb_array.melt;
          symb_ls.melt; Asl_term.to_melt t2; symb_rs.melt ]
      | Free(v, n) ->
        Latex.concat
          [ keyw_free.melt; symb_lp.melt; Asl_term.to_melt v; symb_comma.melt; Asl_term.to_melt n; symb_rp.melt ]
      | Assign(x,e) ->
        Latex.concat
          [ Asl_term.to_melt x; symb_assign.melt; Asl_term.to_melt e ]
      | Load(x, y, e) ->
        Latex.concat
          [ Asl_term.to_melt x; symb_assign.melt; Asl_term.to_melt y;
            symb_ls.melt; Asl_term.to_melt e; symb_rs.melt ]
      | Store(y, e, x) ->
        Latex.concat
          [  Asl_term.to_melt y; symb_ls.melt; Asl_term.to_melt e; symb_rs.melt;
            symb_assign.melt; Asl_term.to_melt x ]
      (*| ProcCall(p, args) ->
        let separated_args = 
          Blist.intersperse symb_comma.melt
            (Blist.map (fun x -> Sl_term.to_melt x) args) in
        Latex.concat (ltx_text p :: symb_lp.melt :: separated_args @ [symb_rp.melt])*)
      | If(cond,cmd) ->
        Latex.concat
          [ keyw_if.melt; ltx_math_space; Cond.to_melt cond; ltx_math_space;
            keyw_then.melt; ltx_math_space; to_melt_label (Blist.hd cmd);
            Latex.ldots; keyw_fi.melt ]
      | IfElse(cond,cmd,cmd') ->
        Latex.concat
          [ keyw_if.melt; ltx_math_space; Cond.to_melt cond; ltx_math_space;
            keyw_then.melt; ltx_math_space; to_melt_label (Blist.hd cmd);
            Latex.ldots; keyw_else.melt; ltx_math_space; 
            to_melt_label (Blist.hd cmd'); Latex.ldots; keyw_fi.melt ]
      | While(cond,cmd) ->
        Latex.concat
          [ keyw_while.melt; ltx_math_space; Cond.to_melt cond; ltx_math_space;
            keyw_do.melt; ltx_math_space; to_melt_label (Blist.hd cmd);
            Latex.ldots; keyw_od.melt ]
    and to_melt_lcmd c = Latex.concat [to_melt_label c; to_melt_cmd c]
    and to_melt = function
      | [] -> Latex.epsilon
      | [ c ] -> to_melt_lcmd c
      | hd::tl ->
        Latex.concat
          [ to_melt_lcmd hd; symb_semicolon.melt;
          to_melt_label (Blist.hd tl); Latex.ldots ]

  end

let program_pp fmt cmd =
  Format.fprintf fmt "%a" (Cmd.pp 0) cmd

let pp_cmd fmt cmd =
  Cmd.pp ~abbr:true 0 fmt cmd

module Seq =
  struct
    type t = Asl_form.t * Cmd.t

    let tagset_one = Tags.singleton 1
		let tagpairs_one = TagPairs.mk tagset_one
    let tags (f,cmd) = tagset_one
    let tag_pairs f = TagPairs.mk (tags f)
    let vars (l,_) = Asl_form.vars l
    let terms (l,_) = Asl_form.terms l
    let subst theta (l,cmd) = (Asl_form.subst theta l, cmd)
    let to_string (f,cmd) =
      (Asl_form.to_string f) ^ symb_turnstile.sep ^ (Cmd.to_string cmd)
    let to_melt (f,cmd) =
      ltx_mk_math
        (Latex.concat [ Asl_form.to_melt f; symb_turnstile.melt; Cmd.to_melt cmd ])

    let pp fmt (f,cmd) =
      Format.fprintf fmt "@[%a%s%a@]"
        Asl_form.pp f symb_turnstile.sep (Cmd.pp ~abbr:true 0) cmd

    let equal (f,cmd) (f',cmd') = 
      Cmd.equal cmd cmd' && Asl_form.equal f f'
    let equal_upto_tags (f,cmd) (f',cmd') = 
      Cmd.equal cmd cmd' && Asl_form.equal_upto_tags f f'
    
    let subsumed (f,cmd) (f',cmd') = 
      Cmd.equal cmd cmd' &&
      (Asl_form.subsumed_upto_tags) 
        ~total:false  f' f 
    let subsumed_upto_tags (f,cmd) (f',cmd') = 
      Cmd.equal cmd cmd' &&
      Asl_form.subsumed_upto_tags ~total:false f' f 
  end

let program_vars = ref Asl_term.Set.empty

let set_program p =
  program_vars := Cmd.vars p

let vars_of_program () = !program_vars

(* remember prog vars when introducing fresh ones *)
let fresh_fvar s = Asl_term.fresh_fvar (Asl_term.Set.union !program_vars s)
let fresh_fvars s i = Asl_term.fresh_fvars (Asl_term.Set.union !program_vars s) i
let fresh_evar s = Asl_term.fresh_evar (Asl_term.Set.union !program_vars s)
let fresh_evars s i = Asl_term.fresh_evars (Asl_term.Set.union !program_vars s) i

(* precondition: PRECONDITION; COLON; f = formula; SEMICOLON { f } *)
let parse_precondition st = 
  ( parse_symb keyw_precondition >>
    parse_symb symb_colon >>
    Asl_form.parse >>= (fun f ->
    parse_symb symb_semicolon >>$ f) <?> "Precondition") st

(* optional multi-line comment (limited by "--"); p = precondition; cmd = command; EOF { (p, cmd) } *)
let parse st = 
  ( optional (spaces >> skip_string "--" >> spaces >> skip_many_until any_char_or_nl ( spaces >> skip_string "--" >> spaces )) >>
    parse_precondition >>= (fun p ->
    Cmd.parse >>= (fun cmd ->
    eof >>$ (p,cmd))) <?> "program") st

let of_channel c =
  handle_reply (parse_channel parse c ())
