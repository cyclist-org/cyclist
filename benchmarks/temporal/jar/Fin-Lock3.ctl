fields: next;
precondition: lock->zero * ls(x,nil);
property:AG(AF(/\(lock->one * emp,<>lock->one)));
lock.next:=one;
while x!=nil do
      temp:=x;
      x:=x.next;
      free(temp)
od;
lock.next:=zero