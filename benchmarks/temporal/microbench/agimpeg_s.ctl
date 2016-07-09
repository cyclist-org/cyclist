fields: next;
precondition: current=zero * check=zero * p=one * zero->one * one->nil  * FALSE->false' * TRUE->true';
property: AG(\/(check=zero,EG(p=one)));
while TRUE=TRUE do
  check:=one;
  if * then
    p:=one
  else
    p:=one
  fi
od