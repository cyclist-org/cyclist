val expect_before :
  ('a, 'b) MParser.t -> ('c, 'b) MParser.t -> string -> ('a, 'b) MParser.t

val parse_pair :
  ('a, 'b) MParser.t -> ('c, 'b) MParser.t -> ('a * 'c, 'b) MParser.t
