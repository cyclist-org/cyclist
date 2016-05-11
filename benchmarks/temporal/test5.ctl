fields: this;
precondition: x->a * y->b;
property: <><>x->b;
free(x); x:=y; skip
