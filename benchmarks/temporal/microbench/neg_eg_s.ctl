fields: next;
precondition: x=one * n->one * one->zero * zero->nil * FALSE->false' * TRUE->true';
property: EG(x!=zero);
while TRUE=TRUE do
  if * then
    x:=one
  else
    x:=zero
  fi
od;
while TRUE=TRUE do
  skip
od
