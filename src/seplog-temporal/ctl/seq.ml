open Lib
open Generic
open While.Program

let is_prog_var v = Seplog.Term.is_free_var v
let is_prog_term t = Seplog.Term.is_nil t || is_prog_var t

type t = Seplog.Form.t * Cmd.t * Form.t

let termination = ref true

let tagset_one = Tags.singleton Tags.anonymous
let tagpairs_one = Tagpairs.mk tagset_one
let tags (sf,cmd,tf) = Tags.union (Seplog.Form.tags sf) (Form.tags tf)
let tag_pairs (sf,_,tf) =
  if !termination then
    Tagpairs.union
      (Seplog.Form.tag_pairs sf)
      (Tagpairs.mk (Form.outermost_tag tf))
  else
    Tagpairs.mk (Form.outermost_tag tf)
let sep_vars (sf,_,_) = Seplog.Form.vars sf
let temp_vars (_,_,tf) = Form.vars tf
let vars (sf,_,tf) = Seplog.Term.Set.union (Seplog.Form.vars sf) (Form.vars tf)
let terms (l,_) = Seplog.Form.terms l
let subst theta (sf,cmd,tf) = (Seplog.Form.subst theta sf, cmd, tf)
let to_string (sf,cmd,tf) =
  (Seplog.Form.to_string sf)
    ^ Symbols.symb_turnstile.sep ^ (Cmd.to_string cmd)
    ^ Symbols.symb_colon.sep ^ (Form.to_string tf)

let pp fmt (sf,cmd,tf) =
  Format.fprintf fmt "@[%a%s%a%s%a@]"
    Seplog.Form.pp sf
    Symbols.symb_turnstile.sep
    (Cmd.pp ~abbr:true 0) cmd
    Symbols.symb_colon.sep
	Form.pp tf

let equal (sf,cmd,tf) (sf',cmd',tf') =
  Cmd.equal cmd cmd' && Seplog.Form.equal sf sf' && Form.equal tf tf'

let equal_upto_tags (sf,cmd,tf) (sf',cmd',tf') =
  Cmd.equal cmd cmd' &&
  Seplog.Form.equal_upto_tags sf sf' &&
  Form.equal_upto_tags tf tf'


let subsumed (sf,cmd,tf) (sf',cmd',tf') =
  if (not (Cmd.equal cmd cmd')) then
    false
  else if !termination then
    Seplog.Form.subsumed ~total:false  sf' sf
  else
    Seplog.Form.subsumed_upto_tags ~total:false  sf' sf
let subsumed_upto_tags (sf,cmd,tf) (sf',cmd',tf') =
  Cmd.equal cmd cmd' &&
  Seplog.Form.subsumed_upto_tags ~total:false sf' sf

let subst_tags tagpairs (sf,cmd,tf) =
  (Seplog.Form.subst_tags tagpairs sf, cmd,tf)
