fields: next;
precondition: x=one * zero->one * one->two * two->three * three->four * four->five * TRUE->true';
property: EG(AG(x=one));
if * then
  x:=one
else
  skip
fi;
while TRUE=TRUE do
  skip
od
