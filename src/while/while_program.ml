
open Lib
open Symbols
open MParser

module SH = Sl_heap

let termination = ref false

module Field =
  struct
    type t = string

    let _map = ref Strng.Map.empty
    let _pam = ref Int.Map.empty

    let add f =
      let next_idx = Strng.Map.cardinal !_map in
      _map := Strng.Map.add f next_idx !_map ;
      _pam := Int.Map.add next_idx f !_pam

    let get_index f = Strng.Map.find f !_map

    let get_fields () = Blist.map snd (Int.Map.to_list !_pam)

    let get_no_fields () = Strng.Map.cardinal !_map

    let pp fmt () =
      Format.fprintf fmt "@[%s%s %s%s@]"
        keyw_fields.str symb_colon.str (Strng.FList.to_string (get_fields ())) symb_semicolon.str

    let reset () =
      _map := Strng.Map.empty ;
      _pam := Int.Map.empty

    let to_melt f = Latex.texttt (Latex.text f)
    
    let parse st = parse_ident st
  end

exception WrongCmd

let is_prog_var v = Sl_term.is_free_var v
let is_prog_term t = Sl_term.is_nil t || is_prog_var t

module Cond =
  struct
    type t =
      | Eq of Sl_term.t * Sl_term.t
      | Deq of Sl_term.t * Sl_term.t
      | Non_det

    let mk_eq e1 e2 = assert(is_prog_term e1); assert(is_prog_term e2); Eq(e1,e2)
    let mk_deq e1 e2 = assert(is_prog_term e1); assert(is_prog_term e2); Deq(e1,e2)
    let mk_non_det () = Non_det

    let is_deq = function
      | Deq(_, _) -> true
      | Eq _ | Non_det -> false
    let is_eq = function
      | Eq(_, _) -> true
      | Deq _ | Non_det -> false
    let is_non_det = function
      | Non_det -> true
      | Eq _ | Deq _ -> false
    let is_det c = not (is_non_det c)

    let dest = function
      | Eq(e1, e2) | Deq(e1, e2) -> (e1,e2)
      | Non_det -> raise WrongCmd

    let terms = function
      | Non_det -> Sl_term.Set.empty
      | Deq(x,y) | Eq(x,y) -> Sl_term.Set.add x (Sl_term.Set.singleton y)

    let vars cond = Sl_term.filter_vars (terms cond)

    let equal cond cond' = match (cond, cond') with
      | (Non_det, Non_det) -> true
      | (Eq(x,y), Eq(x',y')) | (Deq(x,y), Deq(x',y')) ->
        Sl_term.equal x x' && Sl_term.equal y y'
      | _ -> false

    let subst theta cond = match cond with
      | Eq(x, y) -> Eq ((Sl_subst.apply theta x), (Sl_subst.apply theta y))
      | Deq(x, y) -> Deq ((Sl_subst.apply theta x), (Sl_subst.apply theta y))
      | _ -> cond

    let pp fmt = function
      | Non_det ->
        Format.fprintf fmt "@[%s@]" symb_star.str
      | Eq(x,y) ->
        Format.fprintf fmt "@[%a%s%a@]" Sl_term.pp x symb_eq.str Sl_term.pp y
      | Deq(x,y) ->
        Format.fprintf fmt "@[%a%s%a@]" Sl_term.pp x symb_deq.str Sl_term.pp y

    let to_melt = function
      | Non_det -> symb_star.melt
      | Eq(x,y) -> Latex.concat [Sl_term.to_melt x; symb_eq.melt; Sl_term.to_melt y]
      | Deq(x,y) -> Latex.concat [Sl_term.to_melt x; symb_deq.melt; Sl_term.to_melt y]

    let fork f c =
      if is_non_det c then (f,f) else
      let pair = dest c in
      let f' =  SH.with_eqs f (Sl_uf.add pair f.SH.eqs) in
      let f'' = SH.with_deqs f (Sl_deqs.add pair f.SH.deqs) in
      let (f',f'') = if is_deq c then (f'',f') else (f',f'') in
      (f',f'')
      
    let validated_by h = function
      | Eq(x, y) -> Sl_heap.equates h x y
      | Deq(x, y) -> Sl_heap.disequates h x y
      | _ -> false
    
    let invalidated_by h = function
      | Eq(x, y) -> Sl_heap.disequates h x y
      | Deq(x, y) -> Sl_heap.equates h x y
      | _ -> false
    
    let parse st =
      ( attempt (parse_symb symb_star >>$ mk_non_det ()) <|>
        attempt (Sl_uf.parse |>> Fun.uncurry mk_eq) <|>
                (Sl_deqs.parse |>> Fun.uncurry mk_deq) <?> "Cond") st
  end


module Cmd =
  struct
    type cmd_t =
      | Stop
      | Return
      | Skip
      | Assign of Sl_term.t * Sl_term.t
      | Load of Sl_term.t * Sl_term.t * Field.t
      | Store of Sl_term.t * Field.t * Sl_term.t
      | New of Sl_term.t
      | Free of Sl_term.t
      | If of Cond.t * t
      | IfElse of Cond.t * t * t
      | While of Cond.t * t
      | ProcCall of string * Sl_term.FList.t
      | Assert of Sl_form.t
    and basic_t = { label:int option; cmd:cmd_t }
    and t = basic_t list

    let get_cmd c = if c=[] then raise WrongCmd else (Blist.hd c).cmd
    let get_cont c = if c=[] then raise WrongCmd else Blist.tl c
    
    let split c = 
      if Blist.length c < 2 
        then raise WrongCmd
        else ([Blist.hd c], Blist.tl c)

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
    let is_return c = is_not_empty c && match get_cmd c with
      | Return -> true
      | _ -> false
    let is_skip c = is_not_empty c && match get_cmd c with
      | Skip -> true
      | _ -> false
    let is_proc_call c = is_not_empty c && match get_cmd c with
      | ProcCall(_, _) -> true
      | _ -> false
    let is_assert c = is_not_empty c && match get_cmd c with
      | Assert(_) -> true
      | _ -> false 
    

    let is_basic c = is_not_empty c && match get_cmd c with
      | ProcCall(_, _) | Assign _ | Load _ | Store _ | New _ | Free _ | Stop | Skip -> true
      | _ -> false

    let is_final = Fun.list_disj [ is_empty; is_stop; is_return; ]

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
    let mk_load x e s =  mk_basic (Load(x,e,s))
    let mk_store e1 s e2 = mk_basic (Store(e1,s,e2))
    let mk_new x = mk_basic (New(x))
    let mk_free e = mk_basic (Free(e))
    let mk_stop = mk_basic (Stop)
    let mk_return = mk_basic (Return)
    let mk_skip = mk_basic (Skip)
    let mk_proc_call p args = mk_basic (ProcCall(p, args))
    let mk_assert f = mk_basic (Assert(f))
    let mk_if cond cmd = mk_basic (If(cond, cmd))
    let mk_ifelse cond cmd cmd' = mk_basic (IfElse(cond, cmd, cmd'))
    let mk_while cond cmd = mk_basic (While(cond, cmd))
    let mk_seq cmd cmd' = cmd @ cmd'
    let mk_from_list l = Blist.flatten l

    let rec parse_cmd_opt st = 
      (   attempt (parse_symb keyw_stop >>$ Some Stop)
      <|> attempt (parse_symb keyw_return >>$ Some Return)
      <|> attempt (parse_symb keyw_skip >>$ Some Skip)
      <|> attempt (parse_symb keyw_assert >>
          Tokens.parens Sl_form.parse |>> (fun f ->
          Some (Assert(f))))
      <|> attempt (parse_symb keyw_free >>
          Tokens.parens Sl_term.parse |>> (fun v ->
          assert (is_prog_var v) ; Some (Free v)))
      <|> attempt (parse_symb keyw_if >>
          Cond.parse >>= (fun cond ->
          parse_symb keyw_then >>
          parse >>= (fun cmd1 ->
          parse_symb keyw_else >>
          parse >>= (fun cmd2 ->
          parse_symb keyw_fi >>$ Some (IfElse(cond,cmd1,cmd2))))))
      <|> attempt (parse_symb keyw_if >>
          Cond.parse >>= (fun cond ->
          parse_symb keyw_then >>
          parse >>= (fun cmd ->
          parse_symb keyw_fi >>$ Some (If(cond,cmd)))))
      <|> attempt (parse_symb keyw_while >>
          Cond.parse >>= (fun cond ->
          parse_symb keyw_do >>
          parse >>= (fun cmd ->
          parse_symb keyw_od >>$ Some (While(cond,cmd)))))
  (*   | v = var; FLD_SEL; fld = IDENT; ASSIGN; t = term *)
      <|> attempt (parse_ident >>= (fun v ->
          parse_symb symb_fld_sel >>
          parse_ident >>= (fun id ->
          parse_symb symb_assign >>
          Sl_term.parse |>> (fun t ->
          let v = Sl_term.of_string v in
          assert (is_prog_var v) ; Some (Store(v,id,t))))))
  (*   v = var; ASSIGN; NEW; LP; RP { P.Cmd.mk_new v } *)
      <|> attempt (parse_ident <<
          parse_symb symb_assign <<
          parse_symb keyw_new <<
          parse_symb symb_lp <<
          parse_symb symb_rp |>> (fun v ->
          let v = Sl_term.of_string v in
          assert (is_prog_var v) ; Some (New v)))
  (*   | v1 = var; ASSIGN; v2 = var; FLD_SEL; fld = IDENT *)
      <|> attempt (parse_ident >>= (fun v1 ->
          parse_symb symb_assign >>
          parse_ident >>= (fun v2 ->
          parse_symb symb_fld_sel >>
          parse_ident |>> (fun id ->
          let v1 = Sl_term.of_string v1 in
          let v2 = Sl_term.of_string v2 in
          assert (is_prog_var v1 && is_prog_var v2) ; Some (Load(v1,v2,id))))))
    (* | v = var; ASSIGN; t = term { P.Cmd.mk_assign v t } *)
      <|> attempt (parse_ident >>= (fun v -> 
          parse_symb symb_assign >> 
          parse_ident |>> (fun t -> 
          let v = Sl_term.of_string v in
          let t = Sl_term.of_string t in
          assert (is_prog_var v) ; Some (Assign(v,t)))))
      <|> attempt (parse_ident >>= (fun p ->
          Tokens.parens (Tokens.comma_sep Sl_term.parse) |>> (fun args ->
          (Blist.iter (fun arg -> assert(is_prog_var arg || Sl_term.is_nil arg)) args);
          Some (ProcCall(p, args)))))
      <|> (spaces >> string "/*" >> 
          ((many_until any_char_or_nl (string "*/" << spaces)) >>$ None))
      <?> "Cmd") st
    and parse st = 
      (sep_by1 parse_cmd_opt (parse_symb symb_semicolon) >>= (fun cmds ->
      let cmds = Option.list_get cmds in 
      if Blist.is_empty cmds then zero else 
      return (Blist.map mklc cmds)) <?> "CmdList") st

    let _dest_stop = function
      | Stop -> ()
      | _ -> raise WrongCmd
    let _dest_return = function
      | Return -> ()
      | _ -> raise WrongCmd
    let _dest_skip = function
      | Skip -> ()
      | _ -> raise WrongCmd
    let _dest_assign = function
      | Assign(x,e) -> (x,e)
      | _ -> raise WrongCmd
    let _dest_load = function
      | Load(x,e,s) -> (x,e,s)
      | _ -> raise WrongCmd
    let _dest_store = function
      | Store(e1,s,e2) -> (e1,s,e2)
      | _ -> raise WrongCmd
    let _dest_new = function
      | New(x) -> x
      | _ -> raise WrongCmd
    let _dest_free = function
      | Free(e) -> e
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
    let _dest_proc_call = function
      | ProcCall(p, args) -> (p, args)
      | _ -> raise WrongCmd
    let _dest_deref = function
      | Load(x,e,s) -> e
      | Store(e1,s,e2) -> e1
      | Free(e) -> e
      | _ -> raise WrongCmd
    let _dest_branching = function
      | While(cond,_) | If(cond,_) | IfElse(cond,_,_) -> cond
      | _ -> raise WrongCmd 
    let _dest_assert = function
      | Assert(f) -> f
      | _ -> raise WrongCmd

    let dest_cmd f = fun c -> f (get_cmd c)

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
    let dest_branching = dest_cmd _dest_branching
    let dest_proc_call = dest_cmd _dest_proc_call
    let dest_assert = dest_cmd _dest_assert
    let dest_empty c = if c=[] then () else raise WrongCmd

    let number c =
      let rec aux n = function
        | [] -> ([], n)
        | c::l ->
          begin match c.cmd with
            | ProcCall(_,_) | Assign _ | Load _ | Store _ | New _ | Free _ 
            | Stop | Return | Skip ->
              let c' = { label=Some n; cmd=c.cmd } in
              let (l', n') = aux (n+1) l in
              (c'::l', n')
            | Assert(_) ->
              let (l', n') = aux n l in
              (c::l', n')
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
      | Stop | Return | Skip | Assert(_) -> Sl_term.Set.empty
      | New(x) | Free(x) -> Sl_term.Set.singleton x
      | Assign(x,e) | Load(x,e,_) | Store(x,_,e) -> Sl_term.Set.of_list [x; e]
      | If(cond,cmd) -> Sl_term.Set.union (Cond.vars cond) (terms cmd)
      | IfElse(cond,cmd,cmd') ->
        Sl_term.Set.union (Sl_term.Set.union (Cond.vars cond) (terms cmd)) (terms cmd')
      | While(cond,cmd) -> Sl_term.Set.union (Cond.vars cond) (terms cmd)
      | ProcCall(p, args) -> Sl_term.Set.of_list args
    and terms l =
      Blist.fold_left (fun s c -> Sl_term.Set.union s (cmd_terms c.cmd)) Sl_term.Set.empty l

    let vars cmd = Sl_term.filter_vars (terms cmd)
    
    let locals params cmd = Sl_term.Set.diff (vars cmd) params

    let rec cmd_modifies ?(strict=true) cmd = match cmd with
      | Stop | Return | Skip | Free _ | Assert(_) | ProcCall(_, _) -> Sl_term.Set.empty
      | New(x) | Assign(x,_) | Load(x,_,_) -> Sl_term.Set.singleton x
      | Store(x,_,_) -> if strict then Sl_term.Set.singleton x else Sl_term.Set.empty 
      | If(_,cmd) | While(_,cmd) -> modifies ~strict cmd
      | IfElse(_,cmd,cmd') -> 
          Sl_term.Set.union (modifies ~strict cmd) (modifies ~strict cmd')
    and modifies ?(strict=true) l =
      Blist.fold_left 
        (fun s c -> Sl_term.Set.union s (cmd_modifies ~strict c.cmd)) Sl_term.Set.empty l
        
    let rec cmd_equal cmd cmd' = match (cmd, cmd') with
      | (Stop, Stop) | (Return, Return) | (Skip, Skip) -> true
      | (New(x), New(y)) | (Free(x), Free(y)) -> Sl_term.equal x y
      | (Assign(x,e), Assign(x',e')) -> Sl_term.equal x x' && Sl_term.equal e e'
      | (Load(x,e,f), Load(x',e',f')) | (Store(x,f,e), Store(x',f',e')) ->
        Sl_term.equal x x' && Sl_term.equal e e' && f=f'
      | (ProcCall(p, args), ProcCall(p', args')) -> 
        p=p' && Blist.equal Sl_term.equal args args'
      | (While(cond,cmd), While(cond',cmd')) | (If(cond,cmd), If(cond',cmd')) ->
        Cond.equal cond cond' && equal cmd cmd'
      | (IfElse(cond,cmd1,cmd2), IfElse(cond',cmd1',cmd2')) ->
        Cond.equal cond cond' && equal cmd1 cmd1' && equal cmd2 cmd2'
      | (Assert(_), _) | (_, Assert(_)) -> raise WrongCmd
      | _ -> false
    and equal l l' = match (l,l') with
      | ([], []) -> true
      | ([], c::tl) -> 
        (match c.cmd with | Assert(_) -> equal [] tl | _ -> false)  
      | (c::tl, []) -> 
        (match c.cmd with | Assert(_) -> equal tl [] | _ -> false)
      | (c::tl, c'::tl') -> 
        match (c.cmd, c'.cmd) with
        | (Assert(_), _) -> equal tl l'
        | (_, Assert(_)) -> equal l tl'
        | _ -> cmd_equal c.cmd c'.cmd && equal tl tl'

    let rec subst_cmd theta cmd = match cmd with
      | Stop | Return | Skip | Assert(_) -> cmd
      | New(x) -> New (Sl_subst.apply theta x)
      | Free(x) -> Free (Sl_subst.apply theta x)
      | Assign(x, e) -> Assign ((Sl_subst.apply theta x), (Sl_subst.apply theta e))
      | Load(x, e, f) -> Load ((Sl_subst.apply theta x), (Sl_subst.apply theta e), f)
      | Store(x, f, e) -> Store ((Sl_subst.apply theta x), f, (Sl_subst.apply theta e))
      | ProcCall(p, args) -> ProcCall (p, (Sl_term.FList.subst theta args))
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
      | Return -> Format.fprintf fmt "%s" keyw_return.str
      | Skip -> Format.fprintf fmt "%s" keyw_skip.str
      | Assert(f) -> 
        if abbr then
          Format.fprintf fmt "%s%s...%s"
            keyw_assert.str symb_lp.str symb_rp.str
        else
          Format.fprintf fmt "%s%s%a%s"
            keyw_assert.str symb_lp.str Sl_form.pp f symb_rp.str
      | New(x) ->
        Format.fprintf fmt "%a%s%s%s%s"
          Sl_term.pp x symb_assign.sep keyw_new.str symb_lp.str symb_rp.str
      | Free(x) ->
        Format.fprintf fmt "%s%s%a%s"
          keyw_free.str symb_lp.str Sl_term.pp x symb_rp.str
      | Assign(x,e) ->
        Format.fprintf fmt "%a%s%a"
          Sl_term.pp x symb_assign.sep Sl_term.pp e
      | Load(x,e,f) ->
        Format.fprintf fmt "%a%s%a%s%s"
          Sl_term.pp x symb_assign.sep Sl_term.pp e symb_fld_sel.str f
      | Store(x,f,e) ->
        Format.fprintf fmt "%a%s%s%s%a"
          Sl_term.pp x symb_fld_sel.str f symb_assign.sep Sl_term.pp e
      | ProcCall(p, args) -> Format.fprintf fmt "%s(%s)"
          p (Sl_term.FList.to_string_sep symb_comma.sep args)
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

    let rec strip_asserts = function
      | [] -> []
      | c::cont -> 
        (match c.cmd with
          | Assert(_) -> strip_asserts cont
          | _ -> c::cont) 

    let rec to_melt_cmd ?(abbr=true) c = match c.cmd with
      | Stop -> keyw_stop.melt
      | Return -> keyw_return.melt
      | Skip -> keyw_skip.melt
      | Assert(f) ->
        if abbr then Latex.empty else
        Latex.concat
          [ keyw_assert.melt; symb_lp.melt; Sl_form.to_melt f; symb_rp.melt ]
      | New(x) ->
        Latex.concat
          [ Sl_term.to_melt x; symb_assign.melt;
            keyw_new.melt; symb_lp.melt; symb_rp.melt; ]
      | Free(x) ->
        Latex.concat
          [ keyw_free.melt; symb_lp.melt; Sl_term.to_melt x; symb_rp.melt ]
      | Assign(x,e) ->
        Latex.concat
          [ Sl_term.to_melt x; symb_assign.melt; Sl_term.to_melt e ]
      | Load(x,e,f) ->
        Latex.concat
          [ Sl_term.to_melt x; symb_assign.melt; Sl_term.to_melt e;
            symb_fld_sel.melt; Field.to_melt f ]
      | Store(x,f,e) ->
        Latex.concat
          [ Sl_term.to_melt x; symb_fld_sel.melt;
            Field.to_melt f; symb_assign.melt; Sl_term.to_melt e ]
      | ProcCall(p, args) ->
        let separated_args = 
          Blist.intersperse symb_comma.melt
            (Blist.map (fun x -> Sl_term.to_melt x) args) in
        Latex.concat (ltx_text p :: symb_lp.melt :: separated_args @ [symb_rp.melt])
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
    and to_melt_lcmd ?(abbr=true) c = 
      Latex.concat [to_melt_label c; to_melt_cmd ~abbr c]
    and to_melt ?(abbr=true) c = 
      let c = if abbr then strip_asserts c else c in
      match c with
      | [] -> Latex.epsilon
      | [ c ] -> to_melt_lcmd ~abbr c
      | hd::tl ->
        let cont = if abbr then strip_asserts tl else tl in
        let cont_lbl = 
          if is_empty cont then Latex.empty 
            else to_melt_label (Blist.hd cont) in
        Latex.concat
          [ to_melt_lcmd ~abbr hd; symb_semicolon.melt; cont_lbl; Latex.ldots ]

    let rec is_while_prog = function
      | [] -> true
      | c::cs -> match c.cmd with
        | ProcCall(_) | Assert(_) -> false
        | _ -> is_while_prog cs

  end

let program_pp fmt cmd =
  Format.fprintf fmt "%a@\n%a" Field.pp () (Cmd.pp 0) cmd

let pp_cmd fmt cmd =
  Cmd.pp ~abbr:true 0 fmt cmd

module Seq =
  struct
    type t = Sl_form.t * Cmd.t

    let tagset_one = Tags.singleton Tags.anonymous
		let tagpairs_one = Tagpairs.mk tagset_one
    let tags (f,cmd) = if !termination then Sl_form.tags f else tagset_one
    let tag_pairs seq = Tagpairs.mk (tags seq)
    let vars (f,_) = Sl_form.vars f
    let terms (f,_) = Sl_form.terms f
    let subst theta (f,cmd) = (Sl_form.subst theta f, cmd)
    let to_string (f,cmd) =
      (Sl_form.to_string f) ^ symb_turnstile.sep ^ (Cmd.to_string cmd)
    let to_melt (f,cmd) =
      ltx_mk_math
        (Latex.concat [ Sl_form.to_melt f; symb_turnstile.melt; Cmd.to_melt cmd ])

    let pp fmt (f,cmd) =
      Format.fprintf fmt "@[%a%s%a@]"
        Sl_form.pp f symb_turnstile.sep (Cmd.pp ~abbr:true 0) cmd

    let equal (f,cmd) (f',cmd') = 
      Cmd.equal cmd cmd' && Sl_form.equal f f'
    let equal_upto_tags (f,cmd) (f',cmd') = 
      Cmd.equal cmd cmd' && Sl_form.equal_upto_tags f f'
    
    let subsumed (f,cmd) (f',cmd') = 
      Cmd.equal cmd cmd' &&
      (if !termination then Sl_form.subsumed else Sl_form.subsumed_upto_tags) 
        ~total:false  f' f 
    let subsumed_upto_tags (f,cmd) (f',cmd') = 
      Cmd.equal cmd cmd' &&
      Sl_form.subsumed_upto_tags ~total:false f' f 
    
    let subst_tags tagpairs (f,cmd) = (Sl_form.subst_tags tagpairs f, cmd)
  end

let program_vars = ref Sl_term.Set.empty

let set_program p =
  program_vars := Cmd.vars p

let vars_of_program () = !program_vars

(* remember prog vars when introducing fresh ones *)
let fresh_fvar s = Sl_term.fresh_fvar (Sl_term.Set.union !program_vars s)
let fresh_fvars s i = Sl_term.fresh_fvars (Sl_term.Set.union !program_vars s) i
let fresh_evar s = Sl_term.fresh_evar (Sl_term.Set.union !program_vars s)
let fresh_evars s i = Sl_term.fresh_evars (Sl_term.Set.union !program_vars s) i

(* again, treat prog vars as special *)
let freshen_case_by_seq seq case =
  Sl_indrule.freshen (Sl_term.Set.union !program_vars (Seq.vars seq)) case

(* fields: FIELDS; COLON; ils = separated_nonempty_list(COMMA, IDENT); SEMICOLON  *)
(*     { List.iter P.Field.add ils }                                              *)
let parse_fields st = 
  ( parse_symb keyw_fields >>
    parse_symb symb_colon >>
    sep_by1 Field.parse (parse_symb symb_comma) >>= (fun ils ->
    parse_symb symb_semicolon >>$ List.iter Field.add ils) <?> "Fields") st

(* precondition: PRECONDITION; COLON; f = formula; SEMICOLON { f } *)
let parse_precondition ?(allow_tags=false) st = 
  ( parse_symb keyw_precondition >>
    parse_symb symb_colon >>
    (Sl_form.parse ~allow_tags) >>= (fun f ->
    parse_symb symb_semicolon >>$ f) <?> "Precondition") st

    (* fields; p = precondition; cmd = command; EOF { (p, cmd) } *)
let parse st = 
  ( parse_fields >>
    parse_precondition >>= (fun p ->
    Cmd.parse >>= (fun cmd ->
    eof >>$ 
    let p = Sl_form.complete_tags Tags.empty p in
    let theta = Tagpairs.mk_free_subst Tags.empty (Sl_form.tags p) in
    (Sl_form.subst_tags theta p, cmd))) <?> "program") st

let of_channel c =
  handle_reply (parse_channel parse c ())
