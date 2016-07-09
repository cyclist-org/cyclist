fields: next;
precondition: x=one * n->one * one->zero * zero->nil * FALSE->false' * TRUE->true';
property: EG(x!=zero);
x:=zero;
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
x:=one;
while TRUE=TRUE do
  skip
od
