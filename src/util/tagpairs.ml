include Treeset.Make(Pair.Make(Int)(Int))
    
let mk s = Tags.fold (fun i p -> add (i,i) p) s empty

let compose t1 t2 =
  fold
    (fun (x,y) a ->
      fold (fun (w,z) b -> if y=w then add (x,z) b else b) t2 a)
    t1 empty

let projectl tp = map_to Tags.add Tags.empty fst tp
let projectr tp = map_to Tags.add Tags.empty snd tp

let reflect tps = map_to add empty Pair.swap tps

