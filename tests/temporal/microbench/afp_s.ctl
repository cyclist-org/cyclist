fields: next;
precondition: n->one * one->zero * zero->nil * p=zero * current=n * FALSE->false' * TRUE->true';
property: AF(p=one);
while break=FALSE do
  if current=zero then
    break:=TRUE
  else
    current:=current.next
  fi
od;
p:=one;
while TRUE=TRUE do
  skip
od
