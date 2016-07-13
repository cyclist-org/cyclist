fields: next;
precondition: current=zero * check=zero * p=zero * zero->one * one->two * two->three * three->four * four->five * FALSE->false' * TRUE->true' * break=FALSE;
property: EG(\/(x=zero,EF(p=one)));
while TRUE=TRUE do
  check:=zero.next;
  if * then
    while break=FALSE do
      if current=five then
        break:=TRUE
      else
        if * then
	  current:=current.next
	else
	  current:=current.next;
	  current:=current.next
	fi
      fi
    od;
    p:=one
  else
    p:=zero
  fi
od