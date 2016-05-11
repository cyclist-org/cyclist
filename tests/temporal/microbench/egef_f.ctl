fields: next;
precondition: current=zero * x=zero * zero->one * one->two * two->three * three->four * four->five * FALSE->false' * TRUE->true' * break=FALSE;
property: EG(EF(x=one));
while TRUE=TRUE do
  if * then
    while break=FALSE do
     if counter=five then
       break:=TRUE
     else
       if * then
         counter:=counter.next
       else
         counter:=zero
       fi
     fi     
    od;
    x:=zero
  else
    x:=zero
  fi
od;
while TRUE=TRUE do
  skip
od
