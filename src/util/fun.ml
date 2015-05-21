let neg f x = not (f x)
let conj f g x = (f x) && (g x)
let disj f g x = (f x) || (g x)
let id x = x
let uncurry f (x, y) = f x y
let curry f x y = f (x, y)
let swap f x y = f y x

let direct inverse = 
  if not inverse then 
    id
  else
    swap
  
let rec iter f n x = match n with
  | n when n <= 0 -> x
  | n -> iter f (n-1) (f x) 