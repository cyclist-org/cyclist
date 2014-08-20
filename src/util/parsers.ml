open MParser

let try_prefix p continue_with =
  ( (look_ahead p) >> continue_with p )

let expect_before p p' msg st =
  ( (followed_by p' "") >> fail msg
      <|> p ) st
     