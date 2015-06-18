import sys
import gdb
import re
import traceback

identifier_re = re.compile("[_a-zA-Z][_a-zA-Z0-9]*(?=\s=\s.*\n?)")
ptr_re = re.compile("0x[a-f0-9]+(?=\s*(<[_a-zA-Z][_a-zA-Z0-9]*\+\d+>)?)")

def print_err(*args):
    sys.stderr.write(' '.join(map(str,args)) + '\n')
    sys.stderr.flush()
        
def stringify_map(m, sep):
    str = "[ "
    add_comma = False
    for k in m:
        if add_comma: str += ", "
        else: add_comma = True
        str += k
        str += sep
        str += m[k]
    str += " ]"
    return str

def stringify_stack(s):
    return stringify_map(s, " |-> ")

def stringify_heap(h):
    return stringify_map(h, " |-> ")

def stringify_stack_heap_pair(s, h):
    str = "("
    str += stringify_stack(s)
    str += ", "
    str += stringify_heap(h)
    str += ")"
    return str

def is_primitive(val):
    code = val.type.strip_typedefs().code 
    return code == gdb.TYPE_CODE_ENUM or \
         code == gdb.TYPE_CODE_INT or    \
         code == gdb.TYPE_CODE_FLT or    \
         code == gdb.TYPE_CODE_BOOL or   \
         code == gdb.TYPE_CODE_CHAR or   \
         code == gdb.TYPE_CODE_STRING

def is_ptr(val):
    code = val.type.strip_typedefs().code 
    return code == gdb.TYPE_CODE_PTR or \
        code == gdb.TYPE_CODE_REF

def is_struct(val):
    code = val.type.strip_typedefs().code 
    return code == gdb.TYPE_CODE_STRUCT or \
        code == gdb.TYPE_CODE_UNION

def stringify_value(val):
    # For now, let's just take the predefined string value
    # In future, we can output a more decorated form if necessary
    return str(val)
    
def stringify_ptr(ptr):
    # For now, let's just take the predefined string value
    # In future, we can output a more decorated form if necessary
    #print "stringify_ptr"
    #if ptr.type.code == gdb.TYPE_CODE_PTR : print "pointer"
    #if ptr.type.code == gdb.TYPE_CODE_REF : print "reference"
    #print str(ptr)
    return ptr_re.match(str(ptr)).group(0)

def make_cell_string(struct_t, field_vals):
    
    struct_str = struct_t.tag + "("
    add_comma = False
    for fval in field_vals:
        if add_comma : struct_str += ", "
        else : add_comma = True
        struct_str += fval
    struct_str +=  ")"
    
    return struct_str

def get_type_string(code):
    if code == gdb.TYPE_CODE_PTR : return "TYPE_CODE_PTR"
    elif code == gdb.TYPE_CODE_ARRAY : return "TYPE_CODE_ARRAY"
    elif code == gdb.TYPE_CODE_STRUCT : return "TYPE_CODE_STRUCT"
    elif code == gdb.TYPE_CODE_UNION : return "TYPE_CODE_UNION"
    elif code == gdb.TYPE_CODE_ENUM : return "TYPE_CODE_ENUM"
    elif code == gdb.TYPE_CODE_FLAGS : return "TYPE_CODE_FLAGS"
    elif code == gdb.TYPE_CODE_FUNC : return "TYPE_CODE_FUNC"
    elif code == gdb.TYPE_CODE_INT : return "TYPE_CODE_INT"
    elif code == gdb.TYPE_CODE_FLT : return "TYPE_CODE_FLT"
    elif code == gdb.TYPE_CODE_VOID : return "TYPE_CODE_VOID"
    elif code == gdb.TYPE_CODE_SET : return "TYPE_CODE_SET"
    elif code == gdb.TYPE_CODE_RANGE : return "TYPE_CODE_RANGE"
    elif code == gdb.TYPE_CODE_STRING : return "TYPE_CODE_STRING"
    elif code == gdb.TYPE_CODE_ERROR : return "TYPE_CODE_ERROR"
    elif code == gdb.TYPE_CODE_METHOD : return "TYPE_CODE_METHOD"
    elif code == gdb.TYPE_CODE_METHODPTR : return "TYPE_CODE_METHODPTR"
    elif code == gdb.TYPE_CODE_MEMBERPTR : return "TYPE_CODE_MEMBERPTR"
    elif code == gdb.TYPE_CODE_REF : return "TYPE_CODE_REF"
    elif code == gdb.TYPE_CODE_CHAR : return "TYPE_CODE_CHAR"
    elif code == gdb.TYPE_CODE_BOOL : return "TYPE_CODE_BOOL"
    elif code == gdb.TYPE_CODE_COMPLEX : return "TYPE_CODE_COMPLEX"
    elif code == gdb.TYPE_CODE_TYPEDEF : return "TYPE_CODE_TYPEDEF"
    elif code == gdb.TYPE_CODE_NAMESPACE : return "TYPE_CODE_NAMEPSACE"
    elif code == gdb.TYPE_CODE_DECFLOAT : return "TYPE_CODE_DECFLOAT"
    elif code == gdb.TYPE_CODE_INTERNAL_FUNCTION : return "TYPE_CODE_INTERNALFUNCTION"
    else : raise Exception("Type code " + str(code) + " not recognised!")

def harvest_addr(heap, ptr_queue):
    
    ptr, ptr_queue = ptr_queue[0], ptr_queue[1:]
    
   # print "Checking if addr already in the heap"
    if heap.has_key(stringify_ptr(ptr)) :
        return heap, ptr_queue
    #print "Found a fresh addr"
    
    if ptr.type.target().code == gdb.TYPE_CODE_VOID :
        print_err("Encountered void type - ignoring")
        return heap, ptr_queue
    
    try:
        #print "About to dereference pointer"
        val = ptr.referenced_value()
        # It seems that the only way to determine if the memory pointed at by
        # ptr is accessible is by trying to print the referenced value!
        #print "About to check referenced value"
        unicode(val)#.encode('unicode_escape')
    except gdb.MemoryError:
        #print "Caught an exception during dereference"
        #traceback.print_exception(sys.exc_info()[0],
        #                          sys.exc_info()[1],
        #                          sys.exc_info()[2])
        return heap, ptr_queue
    #else:
        #print "Dereferenced pointer successfully: " + unicode(val).encode('unicode_escape')
    
    if is_primitive(val) :
        #print "is primitive"
        heap[stringify_ptr(ptr)] = stringify_value(val)
    elif is_ptr(val) :
        #print "is ptr"
        heap[stringify_ptr(ptr)] = stringify_ptr(val)
        ptr_queue += [val]
    elif is_struct(val) :
        #print "is struct"
        struct_t = val.type
        field_ids = struct_t.fields()
        field_val_strings = []
        for f_id in field_ids :
            #print "field: " + f_id.name
            field_val = val[f_id]
            #print "Field type = " + str(field_val.type.code)
            if is_primitive(field_val) : 
                field_val_strings += [stringify_value(field_val)]
            elif is_ptr(field_val) :
                #print "Adding pointer to the queue"
                field_val_strings += [stringify_ptr(field_val)]
                ptr_queue += [field_val]
            elif is_struct(field_val) :
                #print "About to take address of an inner struct"
                struct_ptr = field_val.address
                if struct_ptr is None : raise "Struct not addressable!"
                field_val_strings += [stringify_ptr(struct_ptr)]
                ptr_queue += [struct_ptr]
        #print "Adding heap cell"
        heap[stringify_ptr(ptr)] = make_cell_string(struct_t, field_val_strings)
    else : raise Exception("Cannot deal with this type of value: " + get_type_string(val.type.code))
    return heap, ptr_queue

def harvest(out, vars):
    
    if not vars :
    
        args_str = gdb.execute("info args", False, True)
        args = identifier_re.findall(args_str)
        
        locals_str = gdb.execute("info locals", False, True)
        locals = identifier_re.findall(locals_str)
        
        vars = args + locals
    
    stack = {}
    ptr_queue = []
    for v in vars:
        val = gdb.parse_and_eval(v.strip())
        stack[v] = stringify_ptr(val) if is_ptr(val) else stringify_value(val) 
        if is_ptr(val) : ptr_queue += [val]
    
    heap = {}
    while len(ptr_queue) != 0 :
        heap, ptr_queue = harvest_addr(heap, ptr_queue)
    
    out.write(stringify_stack_heap_pair(stack, heap))
    out.write('\n')
    