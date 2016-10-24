fields: this;
precondition: one!=zero * true!=false * wakend->true * flag=one;
property: EF (AG (wakend=nil));
while flag=one do
      if sigup=one then
      	 sigup:=zero;
	 skip;
	 if * then
	    flag:=zero
	 else
	    skip
	 fi
       else
         skip
       fi;
       if flag=one then
           if wakend!=nil then
	       free(wakend);
	       skip;
	       skip
	   else
	       skip
	   fi;
	   if wakend=nil then
	       if * then
	           wakend:=true
	       else
	           skip
	       fi
	   else
	       skip
	   fi;
	   if * then
	       flag:=zero
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
