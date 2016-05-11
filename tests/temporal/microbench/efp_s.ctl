fields: next;
precondition: x=one * zero->one * one->two * two->three * three->four * four->five * FALSE->false' * TRUE->true';
property: EF(x=five);
while TRUE=TRUE do
  if * then
    x:=x.next
  else
    x:=zero
  fi
od;
while TRUE=TRUE do
  skip
od