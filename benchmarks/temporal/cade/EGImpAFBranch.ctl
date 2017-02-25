fields: next;
precondition: current=four * check=zero * p=zero * zero->one * one->two * two->three * three->four * four->five * FALSE->false' * TRUE->true' * break=FALSE;
property: EG(\/(x=zero,AF(p=one)));
while TRUE=TRUE do
  check:=zero.next;
  if * then
    while break=FALSE do
      if current=five then
        break:=TRUE
      else
	  current:=current.next
      fi
    od;
    p:=one
  else
    p:=zero
  fi
od