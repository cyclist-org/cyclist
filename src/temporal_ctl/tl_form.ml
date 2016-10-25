open Lib

open Symbols
open MParser
       
module Form : 
sig
  type t =
    | Final of int
    | Atom of Sl_heap.t * int
    | Circle of t * int 
    | Diamond of t * int
    | Box of t * int
    | AG of Tags.elt * t * int
    | EG of Tags.elt * t * int
    | AF of t * int
    | EF of t * int
    | And of t list * int
    | Or of t list * int
  include Utilsigs.BasicType with type t:=t

  val is_final : t -> bool
  val is_atom : t -> bool
  val is_circle : t -> bool
  val is_diamond : t -> bool
  val is_box : t -> bool
  val is_ag : t -> bool
  val is_eg : t -> bool
  val is_af : t -> bool
  val is_ef : t -> bool
  val is_and : t -> bool
  val is_or : t -> bool
  val is_slformula : t -> bool 
  val is_checkable : t -> bool
					 
  val dest_atom : t -> Sl_heap.t
  val dest_circle : t -> t
  val dest_diamond : t -> t
  val dest_box : t -> t
  val dest_ag : t -> Tags.elt * t
  val dest_eg : t -> Tags.elt * t
  val dest_af : t -> t
  val dest_ef : t -> t
  val dest_and : t -> t list
  val dest_or : t -> t list

  val mk_final : t
  val mk_atom : Sl_heap.t -> t
  val mk_circle : t -> t
  val mk_diamond : t -> t
  val mk_box : t -> t
  val mk_ag : Tags.elt -> t -> t
  val mk_eg : Tags.elt -> t -> t
  val mk_af : t -> t
  val mk_ef : t -> t
  val mk_and : t list -> t
  val mk_or : t list -> t
				       
  val unfold_ag : t -> t * t
  val unfold_eg : t -> t * t
  val unfold_af : t -> t * t
  val unfold_ef : t -> t * t
	
	val unfold_or : t -> t * t
	val unfold_and : t -> t * t
			     
  val to_string : t -> string
  val to_melt : t -> Latex.t
		       
  val parse : (t, 'a) MParser.t
  val of_string : string -> t
			      
  (* val norm : t -> t *)
  val to_slformula : t -> Sl_form.t
  val extract_checkable_slformula : t -> Sl_form.t
  val tags : t -> Tags.t
  val outermost_tag : t -> Tags.t
  val subst_tags : Tagpairs.t -> t -> t
  val vars : t -> Sl_term.Set.t
  val equal : t -> t -> bool
  val equal_upto_tags : t -> t -> bool

  val complete_tags : Tags.t -> t -> t
  (** [complete_tags ts f] returns the formula obtained from f by assigning
      all untagged AG/EG occurrences a fresh free tag, avoiding those in [ts].
  *)

  val e_step : t -> t
  val a_step : t -> t
		      
end
  =
  struct
    
    type t =
      | Final of int
      | Atom of Sl_heap.t * int 
      | Circle of t * int
      | Diamond of t * int
      | Box of t * int
      | AG of Tags.elt * t * int
      | EG of Tags.elt * t * int
      | AF of t * int
      | EF of t * int
      | And of t list * int
      | Or of t list * int
			    
    let depth = function
      | Final d | Atom(_,d) | Circle(_,d) | Diamond(_,d) 
      | Box(_,d) | AG(_,_,d) | EG(_,_,d) | AF(_,d) | EF(_,d) 
      | And(_,d) | Or(_,d) -> d

    let hash = Hashtbl.hash

    let is_final = function
      | Final _ -> true
      | _ -> false
    let is_atom = function
      | Atom _ -> true
      | _ -> false
    let is_circle = function
      | Circle _ -> true
      | _ -> false
    let is_diamond = function
      | Diamond _ -> true
      | _ -> false
    let is_box = function
      | Box _ -> true
      | _ -> false
    let is_ag = function
      | AG _ -> true
      | _ -> false
    let is_eg = function
      | EG _ -> true
      | _ -> false
    let is_af = function
      | AF _ -> true
      | _ -> false
    let is_ef = function
      | EF _ -> true
      | _ -> false
    let is_and = function
      | And _ -> true
      | _ -> false
    let is_or = function
      | Or _ -> true
      | _ -> false

    let dest_atom = function
      | Atom(x,_) -> x 
      | _ -> invalid_arg "dest_atom"
    let dest_circle = function
      | Circle(x,_) -> x
      | _ -> invalid_arg "dest_circle"
    let dest_diamond = function
      | Diamond(x,_) -> x
      | _ -> invalid_arg "dest_diamond"
    let dest_box = function
      | Box(x, _) -> x
      | _ -> invalid_arg "dest_box"
    let dest_ag = function
      | AG(ti,x,_) -> (ti,x)
      | _ -> invalid_arg "dest_ag"
    let dest_eg = function
      | EG(ti,x,_) -> (ti,x)
      | _ -> invalid_arg "dest_eg"
    let dest_af = function
      | AF(x,_) -> x
      | _ -> invalid_arg "dest_af"
    let dest_ef = function
      | EF(x,_) -> x
      | _ -> invalid_arg "dest_ef"
    let dest_and = function
      | And(x,_) -> x
      | _ -> invalid_arg "dest_and"
    let dest_or = function
      | Or(x,_) -> x
      | _ -> invalid_arg "dest_or"

    let _mk_final n = (Final n)
    let _mk_atom a n = Atom(a, n)
    let _mk_circle f n = Circle(f, n)
    let _mk_diamond f n = Diamond(f, n)
    let _mk_box f n = Box(f, n)
    let _mk_ag t f n = AG(t, f, n)
    let _mk_eg t f n = EG(t, f, n)
    let _mk_af f n = AF(f, n)
    let _mk_ef f n = EF(f, n)
    let _mk_and fs n = And(fs, n)
    let _mk_or fs n = Or(fs, n)

    let mk_final = _mk_final 1
    let mk_atom a = _mk_atom a 1
    let mk_circle f = _mk_circle f (1 + depth f)
    let mk_diamond f = _mk_diamond f (1 + depth f)
    let mk_box f = _mk_box f (1 + depth f)
    let mk_ag t f = _mk_ag t f (1 + depth f)
    let mk_eg t f = _mk_eg t f (1 + depth f)
    let mk_af f = _mk_af f (1 + depth f)
    let mk_ef f = _mk_ef f (1 + depth f)
    let mk_and fs = 
      _mk_and fs (1 + (List.fold_left (fun d f -> max (depth f) d) 0 fs))
    let mk_or fs = 
      _mk_or fs (1 + (List.fold_left (fun d f -> max (depth f) d) 0 fs))

    let unfold_ag f = match f with
      | AG(_,inner,_) -> (inner,mk_box f)
      | _ -> invalid_arg "unfold_ag"

    let unfold_eg f = match f with
      | EG(_,inner,_) -> (inner,mk_diamond f)
      | _ -> invalid_arg "unfold_eg"

    let unfold_af f = match f with
      | AF(inner,_) -> (inner, mk_box f)
      | _ -> invalid_arg "unfold_af"

    let unfold_ef f = match f with
      | EF(inner,_) -> (inner, mk_diamond f)
      | _ -> invalid_arg "unfold_ef"
		
    let unfold_or f = match f with 
      | Or(fs,i) -> begin match fs with
        | [] | [_] -> invalid_arg "unfold_or"
        | [f;f'] -> (f, f')
        | f::fs_ -> (f, mk_or fs)
		    end
      | _ -> invalid_arg "unfold_or"	
			 
    let unfold_and f = match f with 
      | And(fs,i) -> begin match fs with
        | [] | [_] -> invalid_arg "unfold_and"
        | [f;f'] -> (f, f')
        | f::fs -> (f, mk_and fs)
        end
      | _ -> invalid_arg "unfold_or"	

    let rec fold f acc g = match g with
      | Atom _ | Final _ -> f g acc
      | Circle(h,_) | Diamond(h,_) | Box(h,_)
      | AG(_,h,_) | EG(_,h,_) | AF(h,_) | EF(h,_) -> f g (f h acc)
      | And(fs,_) | Or(fs,_) ->
		     f g (List.fold_left (fun acc h -> fold f acc h) acc fs)

    let for_all f g =
      fold (fun g' acc -> acc && f g') true g

    let exists f g =
      fold (fun g' acc -> acc || f g') false g

    let rec _to_string f = 
      (if is_and f || is_or f then bracket else Fun.id) (to_string f)
    and to_string = 
      let tag_to_string t = if Tags.is_anonymous t then "" else "_" ^ sqbracket (Tags.Elt.to_string t) in
      function
      | Final(i) -> "final"
      | Atom(a,_) -> Sl_heap.to_string a  
      | Circle(f,_) -> "()" ^ (_to_string f)
      | Diamond(f,_) -> "<>" ^ (_to_string f)
      (* | Box(tag, f, _) -> "[" ^ (string_of_int tag) ^ "]" ^ (_to_string f) *)
      | Box(f, _) -> "[]" ^ (_to_string f)
      | AG(tag, f, _) -> "AG" ^ (tag_to_string tag) ^ " " ^ (_to_string f)
      | EG(tag, f, _) -> "EG" ^ (tag_to_string tag) ^ " " ^ (_to_string f)
      | AF(f, _) -> "AF " ^ (_to_string f)
      | EF(f, _) -> "EF " ^ (_to_string f)
      | And(fs,_) -> 
	 Blist.to_string 
	   " & " 
	   (fun g -> (if is_or g then bracket else Fun.id) (to_string g)) 
	   fs
      | Or(fs,_) -> 
	 Blist.to_string " | " to_string fs

    let rec to_melt f = 
      match f with
      | Final _ -> symb_final.melt
      | Atom(a,_) -> Sl_heap.to_melt a
      | Circle(f,_) -> Latex.concat [symb_circle.melt; to_melt f]
      | Diamond(f,_) -> Latex.concat [symb_diamond.melt; to_melt f]
      | Box(f,_) -> Latex.concat [symb_box.melt; to_melt f]
      | AG(_,f,_) -> Latex.concat [symb_ag.melt; to_melt f]
      | EG(_,f,_) -> Latex.concat [symb_eg.melt; to_melt f]
      | AF(f,_) -> Latex.concat [symb_af.melt; to_melt f]
      | EF(f,_) -> Latex.concat [symb_ef.melt; to_melt f]
      | And(fs,_) -> Latex.concat (Latex.list_insert symb_and.melt (Blist.map to_melt fs))
      | Or(fs,_) -> Latex.concat (Latex.list_insert symb_or.melt (Blist.map to_melt fs))
				 
    let pp fmt f = Format.fprintf fmt "@[%s@]" (to_string f)
				  
    (* atoms must be minimal to be considered first after unfolding *)
    let rec compare f g = 
      let r = Int.compare (depth f) (depth g) in
      if r<>0 then r else
	match (f,g) with
	| Final _ , Final _ -> 0
	| Atom(a, _), Atom(b, _) -> Sl_heap.compare a b
	| Circle(a,_), Circle(b,_)
	| Diamond(a,_), Diamond(b,_)
	| Box(a,_), Box(b,_)
	| AF(a,_), AF(b,_)
	| EF(a,_), EF(b,_)-> compare a b
	| AG (atag, a, _), AG(btag, b, _)
	| EG (atag, a, _), EG(btag, b, _) ->
	   begin
	     match Tags.Elt.compare atag btag with
	     | 0 -> compare a b
	     | n -> n
	   end
	| And(a,_), And(b,_) | Or(a,_), Or(b,_) -> 
				let rec compare_list al bl =
				  begin
				    match (al,bl) with 
				    | ([], []) -> 0
				    | ([], _) -> -1
				    | (_, []) -> 1
				    | (hd::tl, hd'::tl') -> 
				       begin
					 match compare hd hd' with
					 | 0 -> compare_list tl tl'
					 | n -> n
				       end
				  end
				in compare_list a b
	| Atom _, _ | _, Or _-> -1
	| Or _, _ | _, Atom _ -> 1
	| Circle _, _ | _, And _ -> -1
	| And _, _ | _, Circle _ -> 1
	| Diamond _, _ -> -1
	| Box _, _ -> 1
	| _, _ -> 1
			
    let equal f g = (compare f g)=0

    (* atoms must be minimal to be considered first after unfolding *)
    let rec compare_upto_tags f g = 
      let r = Int.compare (depth f) (depth g) in
      if r<>0 then r else
	match (f,g) with
	| Final _, Final _ -> 0
	| Atom(a, _), Atom(b, _) -> Sl_heap.compare a b
	| Circle(a,_), Circle(b,_)
	| Diamond(a,_), Diamond(b,_)
	| Box(a,_), Box(b,_)
	| AF(a,_), AF(b,_)
	| EF(a,_), EF(b,_)
	| AG (_, a, _), AG(_, b, _)
	| EG (_, a, _), EG(_, b, _) -> compare a b
	| And(a,_), And(b,_) | Or(a,_), Or(b,_) -> 
				let rec compare_list a b = 
				  begin
				    match (a,b) with 
				    | ([], []) -> 0
				    | ([], _) -> -1
				    | (_, []) -> 1
				    | (hd::tl, hd'::tl') -> 
				       begin
					 match compare hd hd' with
					 | 0 -> compare_list tl tl'
				       | n -> n
				       end
				  end
				in compare_list a b
	| Atom _, _ | _, Or _-> -1
	| Or _, _ | _, Atom _ -> 1
	| Circle _, _ | _, And _ -> -1
	| And _, _ | _, Circle _ -> 1
	| Diamond _, _ -> -1
	| Box _, _ -> 1
	| _, _ -> 1
			
    let equal_upto_tags f g = (compare_upto_tags f g)=0

    let is_slformula f = 
      for_all (fun g -> is_atom g || is_or g) f
	      
    let is_checkable f =
			match f with
			| Atom _ -> true
			| Or (l,_) -> List.fold_left (fun acc x -> acc || is_atom x) false l
			| _ -> false
      (*exists (fun g -> is_atom g) f *)
	     
    (* let rec norm = function *)
    (*   (\* flatten consecutive occurences of operators *\) *)
    (*   (\* | Diamond(f,_) when is_diamond f ->  *\) *)
    (*   (\* 	 norm (mk_diamond (dest_diamond f)) *\) *)
    (*   (\* (\\* | Box(i, f, _) when is_box f && i = fst (dest_box f) -> *\\) *\) *)
    (*   (\* (\\* 	 norm (mk_box i (snd (dest_box f))) *\\) *\) *)
    (*   (\* | Box(f, _) when is_box f -> *\) *)
    (*   (\* 	 norm (mk_box (dest_box f)) *\) *)
    (*   | And(conj,_) when List.exists is_and conj -> *)
    (* 	 let h = List.find is_and conj in *)
    (* 	 let newconj = FormSet.union (FormSet.remove h conj) (dest_and h) in  *)
    (* 	 norm (mk_and newconj) *)
    (*   | Or(disj,_) when FormSet.exists is_or disj -> *)
    (* 	 let h = FormSet.find is_or disj in *)
    (* 	 let newdisj = FormSet.union (FormSet.remove h disj) (dest_or h) in *)
    (* 	 norm (mk_or newdisj) *)

    (*   (\* identities about booleans -- unsure if these should be used *\) *)
    (*   (\* | Circle(f,_) when is_and f ->                                *\) *)
    (*   (\*   norm (mk_and (FormSet.endomap mk_circle (dest_and f)))  *\) *)
    (*   (\* | Circle(f,_) when is_or f ->                                 *\) *)
    (*   (\*   norm (mk_or (FormSet.endomap mk_circle (dest_or f)))    *\) *)
    (*   (\* | Diamond(f,_) when is_or f ->                                *\) *)
    (*   (\*   norm (mk_or (FormSet.endomap mk_diamond (dest_or f)))   *\) *)
    (*   (\* | Box(i, f, _) when is_and f ->                               *\) *)
    (*   (\*   norm (mk_and (FormSet.endomap (mk_box i) (dest_and f))) *\) *)

    (*   (\* pass through *\) *)
    (*   | Atom _ as a -> a *)
    (*   | And(fs,_) -> mk_and (FormSet.endomap norm fs)  *)
    (*   | Or(fs,_) -> mk_or (FormSet.endomap norm fs) *)
    (*   | Circle(f,_) -> mk_circle (norm f) *)
    (*   | Diamond(f,_) -> mk_diamond (norm f) *)
    (*   | Box(f, _) -> mk_box (norm f) *)
    (*   | AG(tag,f,_) -> mk_ag tag (norm f) *)
    (*   | EG(tag,f,_) -> mk_eg tag (norm f) *)
    (*   | AF(f,_) -> mk_af (norm f) *)
    (*   | EF(f,_) -> mk_ef (norm f) *)

    (* include Fixpoint(struct type t = Form.t let equal = equal end) *)

    (* let norm f =  *)
    (*   let f' = fixpoint norm f in *)
    (*   if is_or f' then f' else mk_or (FormSet.singleton f')   *)

    let to_formula hs = Ord_constraints.empty, hs

    let rec to_slformula t = 
      match t with
      | Final i -> invalid_arg ((string_of_int i) ^" -AND- to_slformula")
      | Atom(g,_) -> to_formula [g] 
      | Or(fs,_) -> 
          let hs = Blist.fold_right (fun (_, hs) hs_acc -> hs @ hs_acc) (List.map to_slformula fs) [] in 
          to_formula hs
      | And(fs,i) -> invalid_arg ((string_of_int i) ^" -AND- to_slformula " ^ to_string t)
      | Circle(fs,i) -> invalid_arg ((string_of_int i) ^ " -CIRCLE- to_slformula " ^ to_string t)
      | Diamond(fs,i) -> invalid_arg ((string_of_int i) ^" -AND- to_slformula " ^ to_string t)
      | Box(fs,i) -> invalid_arg ((string_of_int i) ^" -BOX- to_slformula " ^ to_string t)
      | AG(_,fs,i) -> invalid_arg ((string_of_int i) ^" -AG- to_slformula " ^ to_string t)
      | EG(_,fs,i) -> invalid_arg ((string_of_int i) ^" -EG- to_slformula " ^ to_string t)
      | AF(fs,i) -> invalid_arg ((string_of_int i) ^" -AF- to_slformula " ^ to_string t)
      | EF(fs,i) -> invalid_arg ((string_of_int i) ^" -EF- to_slformula " ^ to_string t)

    let rec extract_checkable_slformula t = 
      match t with
      | Atom(g,_) -> to_formula [g] 
      | Or(fs,_) ->  
          let hs = Blist.fold_right (fun (_, hs) hs_acc -> hs @ hs_acc) (List.map extract_checkable_slformula fs) [] in 
          to_formula hs
      | _ -> to_formula []

    let rec tags f =
      fold 
        begin fun g acc -> match g with
        | EG (t,f,_) | AG(t,f,_) -> Tags.add t acc  
        | _ -> acc 
        end
        Tags.empty
        f

    let outermost_tag f = 
      match f with
      | Diamond (EG (t,_,_),_)
      | Box (AG (t,_,_),_)
      | EG(t,_,_)
      | AG(t,_,_) -> Tags.singleton t
      | _ -> Tags.empty
	       
    let rec subst_tags tagpairs f = 
      match f with 
      | Final _ | Atom _ -> f
      | Circle (f,d) -> Circle (subst_tags tagpairs f, d)
      | Diamond (f,d) -> Diamond (subst_tags tagpairs f, d)
      | Box (f,d) -> Box (subst_tags tagpairs f, d)
      | AF (f,d) -> AF (subst_tags tagpairs f, d)
      | EF (f,d) -> EF (subst_tags tagpairs f, d)
      | And (fs,d) -> And (List.map (subst_tags tagpairs) fs, d)
      | Or (fs,d) -> Or (List.map (subst_tags tagpairs) fs, d)
      | AG (tag,f,d) ->
        AG (Tagpairs.apply_to_tag tagpairs tag, subst_tags tagpairs f, d)
      | EG (tag,f,d) ->
	 EG (Tagpairs.apply_to_tag tagpairs tag, subst_tags tagpairs f, d)

    let e_step tf = match tf with
      | Circle (f,_)
      | Box (f,_)
      | Diamond (f,_) -> f
      | _ -> let () = debug (fun () -> "e_step with formula " ^ to_string tf) in 
	     invalid_arg "e_step"

    let a_step tf = match tf with 
      | Circle (f,_)
      | Box (f,_) -> f
      | _ -> let () = debug (fun () -> "a_step with formula " ^ to_string tf) in 
	     invalid_arg "a_step"

    let vars f = 
      fold
	begin fun g acc -> match g with
			   | Atom(h,_) -> Sl_term.Set.union (Sl_heap.vars h) acc
			   | _ -> acc
	end
	Sl_term.Set.empty
	f
  
  let complete_tags avoid f =
    let rec _complete_tags avoid f =
      let wrap mk f n avoid =
        let (f, used) = _complete_tags avoid f in
        (mk f n, Tags.union avoid used) in 
      let do_list avoid fs =
        let g (fs, avoid) f = 
          let (f', used) = _complete_tags avoid f in
          (f::fs, Tags.union avoid used) in
        let (fs, used) = Blist.fold_left g ([], avoid) fs in
        (Blist.rev fs, used) in
      match f with
      | Final _ | Atom _ -> (f, avoid) 
      | Circle(f, n) -> wrap _mk_circle f n avoid
      | Diamond(f, n) -> wrap _mk_diamond f n avoid
      | Box(f, n) -> wrap _mk_box f n avoid
      | AF(f, n) -> wrap _mk_af f n avoid
      | EF(f, n) -> wrap _mk_ef f n avoid
      | And(fs, n) -> Pair.map_left (Fun.swap _mk_and n) (do_list avoid fs)
      | Or(fs, n) -> Pair.map_left (Fun.swap _mk_or n) (do_list avoid fs)
      | AG(t, f, n) ->
          let t = if Tags.is_anonymous t then Tags.fresh_fvar avoid else t in
          let (f, used) = _complete_tags (Tags.add t avoid) f in
          (AG(t, f, n), used)
      | EG(t, f, n) ->
          let t = if Tags.is_anonymous t then Tags.fresh_fvar avoid else t in
          let (f, used) = _complete_tags (Tags.add t avoid) f in
          (EG(t, f, n), used)
      in
    Pair.left (_complete_tags (Tags.union avoid (tags f)) f)

    let rec parse_atom st =
      (attempt (parse_symb symb_final >>$ mk_final)
       <|> attempt (parse_symb symb_circle >>
		  parse_atom|>> (fun inner -> mk_circle inner))
       <|> attempt (parse_symb symb_box >>
		      parse_atom|>> (fun inner -> mk_box inner))
       <|> attempt (parse_symb symb_diamond >>
		      parse_atom|>> (fun inner -> mk_diamond inner))
       <|> attempt (parse_symb symb_ag >>
		      Tokens.parens parse_atom|>> (fun inner -> 
						   mk_ag Tags.anonymous inner))
       <|> attempt (parse_symb symb_eg >>
		      Tokens.parens parse_atom|>> (fun inner -> 
						   mk_eg Tags.anonymous inner))
       <|> attempt (parse_symb symb_af >>
		      Tokens.parens parse_atom|>> (fun inner -> mk_af inner))
       <|> attempt (parse_symb symb_ef >>
		      Tokens.parens parse_atom|>> (fun inner -> mk_ef inner))
       <|> attempt (parse_symb symb_or >>
		      Tokens.parens (sep_by1 parse_atom (parse_symb symb_comma) |>> fun (atoms) ->
										    mk_or atoms))
       <|> attempt (parse_symb symb_and >>
		      Tokens.parens (sep_by1 parse_atom (parse_symb symb_comma) |>> fun (atoms) ->
										    mk_and atoms))
       <|> attempt ((Sl_heap.parse ~allow_tags:false) |>> (fun sf -> mk_atom sf))
      ) st
	
    let parse st = 
      (sep_by1 parse_atom (parse_symb symb_or) >>= (fun atoms ->
				    let f = match atoms with 
						  | [] -> mk_atom Sl_heap.empty
						  | [f] -> f
						  | _ -> mk_or atoms in
            return f
	      <?> "tempform")) st		    

    let of_string s =
      handle_reply (MParser.parse_string parse s ())
  end
