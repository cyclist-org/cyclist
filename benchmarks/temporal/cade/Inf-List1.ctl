fields: next;
precondition: ls(y',x) * ls(x,nil);
property: AG(ls(y',x) * ls(x,nil));
while x=x do
    if x!= nil then
      temp:=x;
      x := x.next;
      temp:=nil
    else
      skip
    fi
od