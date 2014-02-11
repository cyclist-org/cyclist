open Lib
open Util

module Term = Symheap.Term
module Atom = Symheap.Heap

module rec Form : 
  sig
    type t =
      | Atom of Atom.t * int
      | Circle of t * int 
      | Diamond of t * int
      | Box of int * t * int
      | And of FormSet.t * int
      | Or of FormSet.t * int
    include BasicType with type t:=t
  
    val is_atom : t -> bool
    val is_circle : t -> bool
    val is_diamond : t -> bool
    val is_box : t -> bool
    val is_and : t -> bool
    val is_or : t -> bool
    val is_modality : t -> bool
    val is_slformula : t -> bool 
    
    val dest_atom : t -> Atom.t
    val dest_circle : t -> t
    val dest_diamond : t -> t
    val dest_box : t -> int * t
    val dest_and : t -> FormSet.t
    val dest_or : t -> FormSet.t
    
    val mk_atom : Atom.t -> t
    val mk_circle : t -> t
    val mk_diamond : t -> t
    val mk_box : int -> t -> t
    val mk_and : FormSet.t -> t
    val mk_or : FormSet.t -> t
    
    val to_string : t -> string
    
    val norm : t -> t
    val to_slformula : t -> Symheap.Form.t
    val tags : FormSet.elt -> Util.Tags.t
    val tag_pairs : t -> TagPairs.t
    val vars : t -> Term.Set.t
    val equal : t -> t -> bool
  end
  =
  struct
    type t =
      | Atom of Atom.t * int 
      | Circle of t * int
      | Diamond of t * int
      | Box of int * t * int
      | And of FormSet.t * int
      | Or of FormSet.t * int
    
    let depth = function
      | Atom(_,d) | Circle(_,d) | Diamond(_,d) 
      | Box(_,_,d) | And(_,d) | Or(_,d) -> d
    
    (* atoms must be minimal to be considered first after unfolding *)
    let rec compare f g = 
      let r = Int.compare (depth f) (depth g) in
      if r<>0 then r else
      match (f,g) with
        | Atom(a, _), Atom(b, _) -> Atom.compare a b
        | Circle(a,_), Circle(b,_) | Diamond(a,_), Diamond(b,_) -> compare a b
        | Box(atag, a,_), Box(btag, b,_) -> 
          begin
            match Int.compare atag btag with
              | 0 -> compare a b
              | n -> n
          end 
        | And(a,_), And(b,_) | Or(a,_), Or(b,_) -> FormSet.compare a b
        | Atom _, _ | _, Or _-> -1
        | Or _, _ | _, Atom _ -> 1
        | Circle _, _ | _, And _ -> -1
        | And _, _ | _, Circle _ -> 1
        | Diamond _, _ -> -1
        | Box _, _ -> 1

    let equal f g = (compare f g)=0
    let hash = Hashtbl.hash
  
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
    let is_and = function
      | And _ -> true
      | _ -> false
    let is_or = function
      | Or _ -> true
      | _ -> false
    let is_modality f = is_circle f || is_diamond f || is_box f

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
      | Box(i, x, _) -> (i, x)
      | _ -> invalid_arg "dest_box"
    let dest_and = function
      | And(x,_) -> x
      | _ -> invalid_arg "dest_and"
    let dest_or = function
      | Or(x,_) -> x
      | _ -> invalid_arg "dest_or"
    
    let mk_atom a = Atom(a,1)
    let mk_circle f = Circle(f, 1 + depth f)
    let mk_diamond f = Diamond(f, 1 + depth f)
    let mk_box i f = Box(i, f, 1 + depth f)
    let mk_and fs = 
      And(fs, 1 + (FormSet.fold (fun f d -> max (depth f) d) fs 0))
    let mk_or fs = 
      Or(fs, 1 + (FormSet.fold (fun f d -> max (depth f) d) fs 0))
    
    let rec fold f acc g = match g with
      | Atom _ -> f g acc
      | Circle(h,_) | Diamond(h,_) | Box(_,h,_) -> f g (f h acc)
      | And(fs,_) | Or(fs,_) ->
        f g (FormSet.fold (fun h acc -> fold f acc h) fs acc)

    let for_all f g =
      fold (fun g' acc -> acc && f g') true g
    
    let exists f g =
      fold (fun g' acc -> acc || f g') false g
            
    let rec _to_string f = 
      (if is_and f || is_or f then bracket else id) (to_string f)
    and to_string = function 
      | Atom(a,_) -> Atom.to_string a  
      | Circle(f,_) -> "()" ^ (_to_string f)
      | Diamond(f,_) -> "<>" ^ (_to_string f)
      | Box(tag, f, _) -> "[" ^ (string_of_int tag) ^ "]" ^ (_to_string f)
      | And(fs,_) -> 
        Blist.to_string 
          " & " 
          (fun g -> (if is_or g then bracket else id) (to_string g)) 
          (FormSet.to_list fs)
      | Or(fs,_) -> 
        Blist.to_string " | " to_string (FormSet.to_list fs)

    let pp fmt f = Format.fprintf fmt "@[%s@]" (to_string f)
        
    let is_slformula f = 
      for_all (fun g -> is_atom g || is_or g) f
    
    let rec norm = function
      (* flatten consecutive occurences of operators *)
      | Diamond(f,_) when is_diamond f -> 
        norm (mk_diamond (dest_diamond f))
      | Box(i, f, _) when is_box f && i = fst (dest_box f) ->
        norm (mk_box i (snd (dest_box f)))
      | And(conj,_) when FormSet.exists is_and conj ->
        let h = FormSet.find is_and conj in
        let newconj = FormSet.union (FormSet.remove h conj) (dest_and h) in 
        norm (mk_and newconj)
      | Or(disj,_) when FormSet.exists is_or disj ->
        let h = FormSet.find is_or disj in
        let newdisj = FormSet.union (FormSet.remove h disj) (dest_or h) in
        norm (mk_or newdisj)
      
      (* identities about booleans -- unsure if these should be used *)
      (* | Circle(f,_) when is_and f ->                                *)
      (*   norm (mk_and (FormSet.endomap mk_circle (dest_and f)))  *)
      (* | Circle(f,_) when is_or f ->                                 *)
      (*   norm (mk_or (FormSet.endomap mk_circle (dest_or f)))    *)
      (* | Diamond(f,_) when is_or f ->                                *)
      (*   norm (mk_or (FormSet.endomap mk_diamond (dest_or f)))   *)
      (* | Box(i, f, _) when is_and f ->                               *)
      (*   norm (mk_and (FormSet.endomap (mk_box i) (dest_and f))) *)
      
      (* pass through *)
      | Atom _ as a -> a
      | And(fs,_) -> mk_and (FormSet.endomap norm fs) 
      | Or(fs,_) -> mk_or (FormSet.endomap norm fs)
      | Circle(f,_) -> mk_circle (norm f)
      | Diamond(f,_) -> mk_diamond (norm f)
      | Box(i, f, _) -> mk_box i (norm f)
  
    let norm f = 
      let f' = fixpoint equal norm f in
      if is_or f' then f' else mk_or (FormSet.singleton f')  
    
    let rec to_slformula = function
      | Atom(g,_) -> [g] 
      | Or(fs,_) -> Blist.flatten (FormSet.map_to_list to_slformula fs) 
      | _ -> invalid_arg "to_slformula"
    
    let tags f =
      fold 
        begin fun g acc -> match g with
          | Box(i,_,_) -> Tags.add i acc
          | _ -> acc 
        end
        Tags.empty
        f
  
    let tag_pairs f = TagPairs.mk (tags f)  
    
    let vars f = 
      fold
        begin fun g acc -> match g with
          | Atom(h,_) -> Term.Set.union (Atom.vars h) acc
          | _ -> acc
        end
        Term.Set.empty
        f    
end
and FormSet : OrderedContainer with type elt=Form.t = MakeSet(Form)

module Seq =
  struct
    type t = Symheap.Form.t * int * Form.t
    
    let vars (l,_,r) = Term.Set.union (Symheap.Form.vars l) (Form.vars r)
    
    let tags (l,_,r) =
      Tags.union (Symheap.Form.tags l) (Form.tags r)
    
    let equal ((l,i,r):t) ((l',i',r'):t) = 
      i=i' && Symheap.Form.equal l l' && Form.equal r r'
    
    let tag_pairs s = TagPairs.mk (tags s)  
    
    let to_string (l,i,r) =
      (Symheap.Form.to_string l) ^ " ||-_" ^ (string_of_int i) ^ " " ^
      (Form.to_string r)
    
    (* let is_pure (_,_,r) = Form.is_pure r *)
    
    let pp fmt s = Format.fprintf fmt "@[%s@]" (to_string s)
    
    let subsumed_wrt_tags tags (l,i,r) (l',i',r') =
      if i<>i' || not (Form.equal r r') then false else 
      (* as RHS is the same we can remove the temporal tags *) 
      let ltags = Tags.diff tags (Form.tags r) in  
      Program.Seq.subsumed_wrt_tags ltags (l,i) (l',i')

    let subst theta (l,i,r) =
      (* it is assumed that no program variables are substituted *) 
      (Symheap.Form.subst theta l, i, r)
      
    (*  s' *)
    (* ___ *)
    (*  s  *)
    let uni_subsumption s s' =
      let ((l,i,r),(l',i',r')) = (s,s') in
      if i<>i' || not (Form.equal r r') then None else
      let commontags = Tags.inter (tags s) (tags s') in
      if Tags.is_empty commontags then None else 
      let valid theta' =
        let s'' = subst theta' s' in
        let tags' = Tags.fold
          ( fun t acc ->
            let new_acc = Tags.add t acc in
            if subsumed_wrt_tags new_acc s s'' then new_acc else acc
          ) commontags Tags.empty in
        if not (Tags.is_empty tags') then Some theta' else None in
      Symheap.Form.spw_left_subsumption valid Term.empty_subst l' l
        
    let norm (l,i,r) = (Symheap.Form.norm l, i, Form.norm r)   
  end

module Case = Symheap.Case
module Defs = Symheap.Defs
