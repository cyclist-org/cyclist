fields: next;
precondition: current=four * x=zero * zero->one * one->two * two->three * three->four * four->five * FALSE->false' * TRUE->true' * break=FALSE;
property: AF(EF(x=one));
while break=FALSE do
  if current=five then
    break:=TRUE
  else
    current:=current.next
  fi
od;
break:=FALSE;
current:=four;
while break=FALSE do
  if current=five then
    break:=TRUE
  else
    if * then
      current:=current.next
    else
      current:=current.next
    fi
  fi
od;
x:=one;
while TRUE=TRUE do
  skip
od
