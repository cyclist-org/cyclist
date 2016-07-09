fields: next;
precondition: current=zero * check=zero * p=zero * zero->one * one->two * two->three * three->four * four->five * FALSE->false' * TRUE->true' * break=FALSE;
property: EG(\/(x=zero,AF(p=one)));
check:=one;
if * then
   while break=FALSE do
     if current=five then
       break:=TRUE
     else
       check:=one;
       current:=current.next
     fi
   od;
   p:=one
else
  skip
fi;
while TRUE=TRUE do
  skip
od