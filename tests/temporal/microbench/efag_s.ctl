fields: next;
precondition: current=zero * x=zero * zero->one * one->two * two->three * three->four * four->five * FALSE->false' * TRUE->true' * break=FALSE;
property: EF(AG(x=one));
while break=FALSE do
  if current=five then
    break:=TRUE
  else
    if * then
      current:=current.next
    else
      skip
    fi
  fi
od;
x:=one;
while TRUE=TRUE do
  skip
od
