fields: next;
precondition: x=zero * zero->one * one->two * two->three * three->four * four->five;
property: AF(x!=zero);
while TRUE=TRUE do
  if * then
    x:=x.next
  else
    x:=zero
  fi
od