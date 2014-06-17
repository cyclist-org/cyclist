   type 'a t = 'a list * 'a list

   let make front back = match front with
     | [] -> (Blist.rev back, [])
     | _  -> (front, back)

   let put el (front, back) = make front (el::back)

   let hd (front, _) = match front with
     | []        -> raise Not_found
     | el::front -> el

   let tl (front, back) = match front with
     | []        -> raise Not_found
     | el::front -> make front back

   let take fq = (hd fq, tl fq)

   let put_front el (front, back) = (el::front, back)

   let is_empty (front, _) = match front with
     | [] -> true
     | _  -> false

   let length (front, back) = (Blist.length front) + (Blist.length back)
   let singleton el = put el ([], [])
   let empty = ([], [])
