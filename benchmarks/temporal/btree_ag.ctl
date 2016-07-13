fields: left, right;
precondition: bt(x) ;
property: AF(final);
while x!=nil do
    x := x.left 
od
