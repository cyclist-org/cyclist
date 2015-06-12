fields: this;
precondition: one!=zero * true!=false * wakend=true * flag=one;
property: EF EG wakend=false;
while flag=one do
      if sigup=positive then
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
       if flag=zero then
           if wakend=true then
	       wakend:=false;
	       skip;
	       skip
	   else
	       skip
	   fi;
	   if wakend=false then
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