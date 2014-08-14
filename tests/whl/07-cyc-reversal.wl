fields: next;
precondition: 
ls(x,w') * ls(y,nil) * w'->i' * ls(i',w') \/
ls(j',nil) * w'->j' * ls(x,w') * ls(y,w') \/
ls(x,nil) * ls(y,w') * w'->k' * ls(k',w');
while x!=nil do
  z := x;
  x := x.next;
  z.next := y;
  y := z
od
