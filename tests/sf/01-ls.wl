fields: next;
precondition: ls(x,nil); 
postcondition: ls(x,nil);
while x!=nil {
  x := x.next
}
