fields: next;
precondition: current=zero * x=zero * zero->one * one->two * two->three * three->four * four->five * FALSE->false' * TRUE->true' * break=FALSE;
property: EG(AF(x=one));
if * then
  while break=FALSE do 
    if current=five then
      break:=TRUE
    else
      current:=current.next
    fi
  od;
  x:=one
else
  x:=one
fi;
while TRUE=TRUE do
  skip
od
