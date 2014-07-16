fields: left, right;
precondition: bt(x);
postcondition: bt(x); 
while x!=nil {
  if * {
    x := x.left 
  } else {
    x := x.right
  }
}