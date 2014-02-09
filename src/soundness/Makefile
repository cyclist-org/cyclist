# Where Spot is installed (cf. --prefix for configure)
SPOT_DIR := /usr/local

# Where the OCaml C header files are located  
OCAML_LIB := /usr/lib/ocaml/caml/

# Where the static version of the stdc++ library is located 
STDCPP := /usr/lib/gcc/x86_64-linux-gnu/4.8/libstdc++.a 




# You shouldn't have to change anything below this line
INCLUDES := -I$(SPOT_DIR)/include/spot/ -I$(OCAML_LIB)
CFLAGS := -O2 -g -Wall -std=c++0x
ARCHIVES := \
$(STDCPP) \
$(SPOT_DIR)/lib/libbdd.a \
$(SPOT_DIR)/lib/libspot.a

SRCS := \
./proof.cpp \
./proof_aut.cpp \
./trace.cpp \
./soundness.cpp

OBJS := $(SRCS:.cpp=.o) 
DEPS := $(SRCS:.cpp=.P)

lib: $(OBJS)
	ar rTs soundness.a $(ARCHIVES) $(OBJS) 

%.o: %.cpp 
	g++ $(INCLUDES) $(CFLAGS) -c -MMD -o "$@" "$<"
	@cp $*.d $*.P; \
	sed -e 's/#.*//' -e 's/^[^:]*: *//' -e 's/ *\\$$//' \
		-e '/^$$/ d' -e 's/$$/ :/' < $*.d >> $*.P; \
	rm -f $*.d

-include $(DEPS)

clean:
	-rm -f $(OBJS) $(DEPS) soundness.a

	
