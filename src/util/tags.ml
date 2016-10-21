open MParser
open MParser_PCRE
module Tokens = MParser_PCRE.Tokens

let classify_varname s =
  let l = String.length s in 
  assert (String.length s > 0);
  if l > 1 && s.[l-1] = '\'' then VarManager.BOUND
  else VarManager.FREE
module VM = (val (VarManager.mk 0 "nil" classify_varname) : VarManager.S)

module Elt =
  struct
    include VM.Var

    let parse st =
      (   Tokens.squares
            ((regexp (make_regexp "[a-z][0-9]*[']?") << spaces) >>=
              (fun name -> return (VM.mk name)))
      <?> "Tag") st

    let to_melt v =
      if VM.is_anonymous v then Latex.empty
      else
      let name = to_string v in
      let is_exist = VM.is_exist_var v in
      let min_len = if is_exist then 2 else 1 in
      let ltx = Symbols.char_to_greek name.[0] in
      let ltx = if is_exist then Symbols.ltx_prime ltx else ltx in
      let ltx =
        if (String.length name) = min_len then ltx
        else
          Latex.index
            ltx
            (Latex.text (String.sub name 1 ((String.length name)-min_len))) in
      Symbols.ltx_mk_math ltx
    
    module Unifier = Treeset.Make(Pair.Make(VM.Var)(VM.Var))

    let unify ?(update_check=Fun._true) t t' cont init_state =
      let pair = Unifier.find_opt (fun p -> equal (fst p) t) init_state in
      let res =
        if Option.is_some pair then 
          Option.mk (equal (snd (Option.get pair)) t') init_state
        else 
          Option.mk_lazily 
            (equal t t' ||
              update_check (init_state, (Unifier.singleton (t, t')))) 
            (fun _ -> Unifier.add (t, t') init_state) in
      Option.bind cont res
      
    let biunify 
        ?(update_check=Fun._true) t t' cont ((subst, subst') as state) =
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
              (equal t' t'' ||
                update_check 
                  (state, 
                    (Unifier.empty, Unifier.singleton (t', t''))))
              (fun _ -> (subst, Unifier.add (t', t'') subst')) ]
        else if Option.is_some rpair then
          let t'' = snd (Option.get rpair) in
          [ Option.mk_lazily
              (equal t t'' ||
                update_check 
                  (state, 
                    (Unifier.singleton (t, t''), Unifier.empty)))
              (fun _ -> (Unifier.add (t, t'') subst, subst')) ]
        else
          [ Option.mk_lazily
              (equal t t' ||
                update_check 
                  (state, 
                    (Unifier.singleton (t, t'), Unifier.empty)))
              (fun _ -> 
                (Unifier.add (t, t') subst, 
                  Unifier.add (t', t') subst')) ;
             Option.mk_lazily
              (equal t t' ||
                update_check 
                  (state, 
                    (Unifier.empty, Unifier.singleton (t', t))))
              (fun _ -> 
                (Unifier.add (t, t) subst, 
                  Unifier.add (t', t) subst')) ] in
      Blist.find_map (Option.bind cont) opts
  end

include Elt.Set
include VM
