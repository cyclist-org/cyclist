include List

let map f xs = rev (rev_map f xs)
let map2 f xs ys = rev (rev_map2 f xs ys)
let append xs ys = rev_append (rev xs) ys
let flatten xs = rev (fold_left (fun ys x -> rev_append x ys) [] xs)
