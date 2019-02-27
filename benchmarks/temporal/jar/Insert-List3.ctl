fields: next;
precondition: false!=true * break=false * x=one * zero->one * one->two * two->three * three->four * four->five;
property: AG(AF(x!=zero));
while break!=true do
  if * then
    x:=x.next
  else
    x:=zero
  fi;
  if x!=zero then
    break:=true
  else
    skip
  fi
od;
while break=break do
  skip
od