open Lib
open Util 

let map = ref Int.Map.empty 
let inv_map = ref Strng.Map.empty
let max_var = ref 0
let min_var = ref 0

let get_limit exist = if exist then !min_var else !max_var
let get_diff exist = if exist then (-1) else 1
         
let present v = Int.Map.mem v !map
let name_present n = Strng.Map.mem n !inv_map 
        
let to_string v = Int.Map.find v !map  
let get_idx n = Strng.Map.find n !inv_map  
        
let is_var t = t<>0
let is_exist_var v = (is_var v) && v<0
let is_univ_var v = (is_var v) && v>0 
let is_valid_var v exist = 
  exist && is_exist_var v || not exist && is_univ_var v
                
let is_exist_name n = n.[(String.length n)-1] = '\''
let is_univ_name n = not (is_exist_name n)
let is_valid_name v exist = 
  exist && is_exist_name v || not exist && is_univ_name v
      
let set v name = 
  require (fun () -> not (present v) && not (name_present name)) ;
  assert 
    (is_exist_var v && is_exist_name name || 
     is_univ_var v && is_univ_name name);
  map := Int.Map.add v name !map ;
  inv_map := Strng.Map.add name v !inv_map ;
  max_var := max !max_var v ;
  min_var := min !min_var v  

let mk_var name exist =
  require (fun () -> is_valid_name name exist);
  if name_present name then
    let v = get_idx name in require (fun () -> is_valid_var v exist) ; v
  else
    let v = (get_diff exist) + (get_limit exist) in 
    set v name ; v

let mk_exist_var name = mk_var name true
let mk_univ_var name = mk_var name false
              
let fresh_varname exist =
  let suffix = if exist then "'" else "" in
  let idx = ref 0 in
  let letter = ref 'a' in
  let gen_name () = 
    (string_of_char !letter) ^ 
    (if !idx = 0 then "" else Printf.sprintf "%i" !idx) ^ 
    suffix in
  let name = ref (gen_name ()) in
  while name_present !name && !letter < 'z' do
    letter := char_of_int (1 + (int_of_char !letter)) ;
    name := gen_name () 
  done ;
  if not (name_present !name) then !name else
  begin
    letter := if exist then 'v' else 'u';
    idx := 1;
    name := gen_name () ;
    while name_present !name do
      incr idx ; 
      name := gen_name () 
    done ;
    assert (not (name_present !name)) ; 
    !name
  end
   
  
        
let fresh_var s exist =
  let d = get_diff exist in
  let limit = abs (get_limit exist) in
  let i = ref d in
  let found = ref false in
  while abs (!i) <= limit && not !found do
    if Int.Set.mem !i s then 
      i := !i + d
    else
      found := true
  done ;
  if !found then !i else mk_var (fresh_varname exist) exist 

let fresh_evar s = fresh_var s true    
let fresh_uvar s = fresh_var s false
  
let rec fresh_vars s i exist = match i with 
  | 0 -> [] 
  | n -> 
    let v = fresh_var s exist in
    v::(fresh_vars (Int.Set.add v s) (n-1) exist)

let fresh_evars s i = fresh_vars s i true
let fresh_uvars s i = fresh_vars s i false

