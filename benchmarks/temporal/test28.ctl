fields: next;
precondition: x=nil;
property: EF(AG(x=nil));
if * then x:=nil else x:=new() fi ; while x=x do skip od