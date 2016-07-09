fields: next;
precondition: current=zero * x=zero * zero->one * one->two * two->three * three->four * four->five * FALSE->false' * TRUE->true' * break=FALSE;
property: AF(EG(x=one));
while break=FALSE do
  if current=five then
    break:=TRUE
  else
    current:=current.next
  fi
od;
x:=one;
while TRUE=TRUE do
  if * then
    x:=zero
  else
    skip
  fi
od
