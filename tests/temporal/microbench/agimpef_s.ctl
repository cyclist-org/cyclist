fields: next;
precondition: current=zero * check=zero * p=zero * zero->one * one->two * two->three * three->four * four->five * FALSE->false' * TRUE->true' * break=FALSE;
property: AG(\/(check=zero,EF(p=one)));
check:=one;
while break=FALSE do
  if current=five then
    break:=TRUE
  else
    check:=one;
    if * then
      current:=current.next
    else
      skip
    fi
  fi
od;
p:=one;
while TRUE=TRUE do
  skip
od