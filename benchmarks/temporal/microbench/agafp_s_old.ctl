fields: next;
precondition: current=four * x=zero * zero->one * one->two * two->three * three->four * four->five * FALSE->false' * TRUE->true';
property: AG(AF(x=one));
if * then
   x:=one
else
  while break=FALSE do
    if current=five then
      break:=TRUE
    else
      current:=current.next
    fi
  od;
  x:=one
fi;
while TRUE=TRUE do
  skip
od
