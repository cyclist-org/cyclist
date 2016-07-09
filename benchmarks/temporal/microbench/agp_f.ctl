fields: next;
precondition: x=one * zero->one * one->two * two->one * FALSE->false' * TRUE->true';
property: AG(x!=zero);
while TRUE=TRUE do
  if * then
    x:=x.next
  else
    x:=zero
  fi
od;
while TRUE=TRUE do
  skip
od