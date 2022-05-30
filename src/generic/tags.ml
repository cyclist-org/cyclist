open MParser
open MParser_RE

open Lib

let classify_varname s =
  let l = String.length s in
  assert (Int.( > ) (String.length s) 0) ;
  if Int.( > ) l 1 && Char.equal s.[l - 1] '\'' then VarManager.BOUND
  else VarManager.FREE

module VM = (val VarManager.mk 0 "_" classify_varname : VarManager.S)

module Elt = struct
  include VM.Var

  let parse st =
    ( Tokens.squares
        ( regexp (make_regexp "[a-z][0-9]*[']?")
        << spaces
        >>= fun name -> return (VM.mk name) )
    <?> "Tag" )
      st

  module Unifier = Treeset.Make (Pair.Make (VM.Var) (VM.Var))

  let unify ?(update_check = Fun._true) t t' cont init_state =
    let pair = Unifier.find_opt (fun p -> equal (fst p) t) init_state in
    let res =
      if Option.is_some pair then
        Option.mk (equal (snd (Option.get pair)) t') init_state
      else
        Option.mk_lazily
          (equal t t' || update_check (init_state, Unifier.singleton (t, t')))
          (fun _ -> Unifier.add (t, t') init_state)
    in
    Option.bind cont res

  let biunify ?(update_check = Fun._true) t t' cont ((subst, subst') as state)
      =
    let lpair = Unifier.find_opt (fun p -> equal (fst p) t) subst in
    let rpair = Unifier.find_opt (fun p -> equal (fst p) t') subst' in
    let opts =
      if Pair.both (Pair.map Option.is_some (lpair, rpair)) then
        [ Option.mk
            (Fun.uncurry equal
               (Pair.map (fun p -> snd (Option.get p)) (lpair, rpair)))
            state ]
      else if Option.is_some lpair then
        let t'' = snd (Option.get lpair) in
        [ Option.mk_lazily
            ( equal t' t''
            || update_check
                 (state, (Unifier.empty, Unifier.singleton (t', t''))) )
            (fun _ -> (subst, Unifier.add (t', t'') subst')) ]
      else if Option.is_some rpair then
        let t'' = snd (Option.get rpair) in
        [ Option.mk_lazily
            ( equal t t''
            || update_check (state, (Unifier.singleton (t, t''), Unifier.empty))
            )
            (fun _ -> (Unifier.add (t, t'') subst, subst')) ]
      else
        [ Option.mk_lazily
            ( equal t t'
            || update_check (state, (Unifier.singleton (t, t'), Unifier.empty))
            )
            (fun _ -> (Unifier.add (t, t') subst, Unifier.add (t', t') subst'))
        ; Option.mk_lazily
            ( equal t t'
            || update_check (state, (Unifier.empty, Unifier.singleton (t', t)))
            )
            (fun _ -> (Unifier.add (t, t) subst, Unifier.add (t', t) subst'))
        ]
    in
    Blist.find_map (Option.bind cont) opts
end

include Elt.Set
include VM
