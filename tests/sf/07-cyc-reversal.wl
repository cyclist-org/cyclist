fields: next;
precondition: 
ls_1(x,w') * ls_2(y,nil) * w'->i' * ls_3(i',w') \/
ls_2(j',nil) * w'->j' * ls_3(x,w') * ls_1(y,w') \/
ls_2(x,nil) * ls_1(y,w') * w'->k' * ls_3(k',w');
while x!=nil do
  z := x;
  x := x.next;
  z.next := y;
  y := z
od
