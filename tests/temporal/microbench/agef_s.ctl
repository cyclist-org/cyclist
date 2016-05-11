fields: next;
precondition: current=zero * x=zero * zero->one * one->two * two->three * three->four * four->five * FALSE->false' * TRUE->true';
property: AG(EF(x=one));
while break=FALSE do
  if current=five then
    break:=TRUE
  else
    current:=current.next
  fi
od;
x:=one;
while TRUE=TRUE do
  skip
od
