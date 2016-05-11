fields: this;
precondition: x=nil;
property: AG(x=nil);
while x=nil do
  if * then
    x := nil
  else
    skip
  fi
od
