fields: next;
precondition: wakend->nil * sigup->a' * flag->b';
property: EF(EG(wakend=nil));
while flag!=nil do
      if sigup!=nil then
	 free(sigup);
      	 sigup:=nil;
	 skip;
	 if * then
	    free(flag);
	    flag:=nil
	 else
	    skip
	 fi
       else
         skip
       fi;
       if flag!=nil then
           if wakend!=nil then
	       free(wakend);
	       wakend:=nil;
	       skip;
	       skip
	   else
	       skip
	   fi;
	   if wakend=nil then
	       if * then
		   wakend:=new();
	           wakend.next:=nil
	       else
	           skip
	       fi
	   else
	       skip
	   fi;
	   if * then
	       free(flag);
	       flag:=nil
	   else
	       skip
	   fi
       else
           skip
       fi
od;
while one=one do
    skip
od
