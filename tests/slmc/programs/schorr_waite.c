#include <stdlib.h>
#include <stdbool.h>

struct node {
  bool m; // marked
  bool c; // which child is explored
  struct node* l;
  struct node* r;
  
};

/*@
predicate tree(struct node* t, bool marked) = 
  t==0 ? true : t->m |-> marked &*& t->c |-> ?c &*& t->l |-> ?l &*& t->r |-> ?r &*& tree(l, marked) &*& tree(r, marked);
  
predicate stack(struct node* t) =
  t == 0 ? true : t->m |-> true &*& t->c |-> ?c &*& t->l |-> ?l &*& t->r |-> ?r &*& (c == false ? stack(l) &*& tree(r, false) : stack(r) &*& tree(l, true));

@*/

void schorr_waite(struct node* root) 
  //@ requires tree(root, false);
  //@ ensures tree(_, true);
{
  struct node* t = root; 
  struct node* p = 0;
  //@ close stack(p);
  //@ open tree(root, false);
  while(p != 0 || (t != 0 && ! (t->m)))
    //@ invariant (t == 0 ? true : t->m |-> ?marked &*& t->c |-> ?c &*& t->l |-> ?l &*& t->r |-> ?r &*& tree(l, marked) &*& tree(r, marked)) &*& stack(p);
  {
    if(t == 0 || t->m) {
      //@ open stack(p);
      if(p->c) { // pop
        struct node* q = t;
        t = p;
        p = p->r;
        t->r = q;
        //@ close tree(q, true); 
      } else { // swing
        struct node* q = t;
        t = p->r;
        p->r = p->l;
        p->l = q;
        p->c = true;
        //@ close tree(q, true);  
        //@ close stack(p);
        //@ open tree(t, false);
      }
    } else { // push
      struct node* q = p;
      p = t;
      t = t->l;
      p->l = q;
      p->m = true;
      p->c = false;
      //@ open tree(t, false);
      //@ close stack(p);
    }
  }
  //@ open stack(p);
  //@ close tree(t, true);
}

struct node * makeNode()
{
	struct node * n = malloc(sizeof(struct node));
	n->m = false;
	n->c = false;
	n->l = 0;
	n-> r = 0;
	return n;
}

void main(int argc, char **argv)
{
	int numnodes = atoi(argv[1]);
	unsigned int seed  = atoi(argv[2]);
	int i = 1;
	struct node * root = makeNode();
	while (i < numnodes) {
		struct node * n = root;
		struct node * p = 0;
		int j = 0;
		while (n) {
			j = rand() % 2;
			p = n;
			if (j) n = n->l;
			else n = n->r;
		}
		if (j) p->l = makeNode();
		else p->r = makeNode();
		i++;
	}
	schorr_waite(root);
}
