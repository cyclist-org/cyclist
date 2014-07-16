fields: next;
precondition: ls(x, nil);
postcondition: emp;
while x!=nil {
	y := x;
	x := x.next;
	free(y)
}
