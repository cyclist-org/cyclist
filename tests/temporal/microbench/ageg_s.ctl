fields: next;
precondition: current=zero * x=one * zero->one * one->two * two->three * three->four * four->five * FALSE->false' * TRUE->true';
property: AG(EG(x!=zero));
while TRUE=TRUE do
  if * then
    x:=one
  else
    x:=two
  fi
od;
while TRUE=TRUE do
  skip
od
