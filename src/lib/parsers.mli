val expect_before :
  ('a, 'b) MParser.t ->
  ('c, 'b) MParser.t ->
  string ->
  'b MParser.state ->
  ('a, 'b) MParser.reply

val parse_pair :
  ('a, 'b) MParser.t ->
  ('c, 'b) MParser.t ->
  'b MParser.state ->
  ('a * 'c, 'b) MParser.reply
