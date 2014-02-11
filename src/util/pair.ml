let map f p = (f (fst p), f (snd p))
let apply f p = f (fst p) (snd p)
let conj p = apply (&&) p
let disj p = apply (||) p
let swap (x,y) = (y,x)
let perm f p = apply f p || apply f (swap p)
let fold f (x,y) a = f y (f x a)
