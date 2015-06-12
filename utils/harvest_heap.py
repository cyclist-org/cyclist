#!/usr/local/archer/bin/gdb -P
import sys
import traceback
import itertools
import gdb
from gdb_harvest.lib import harvest

class HarvesterBP(gdb.Breakpoint):
    
    def __init__(self, spec, filename_base):
        super(HarvesterBP, self).__init__(spec)
        self.filename_gen = filename_base
        self.counter = itertools.count()
        
    def stop(self):
        filename = "{0}{1:04d}.txt".format(self.filename_gen, self.counter.next())
        try:
            with open(filename, 'w') as f:
                harvest(f)
        except:
            os.remove(filename)
            traceback.print_exc()
        return False

if (len(sys.argv) < 1) or ((len(sys.argv) - 1) % 2 != 0):
    print "usage: " + sys.argv[0] + " <file-to-run> [<breakpoint> <output-file-prefix>]+"
    sys.exit()

# Set the file to be executed
gdb.execute("file " + sys.argv[0])

# Set up all the breakpoints
for i in range(1, len(sys.argv), 2):
    HarvesterBP(sys.argv[i], sys.argv[i+1])
    
# Run the file
gdb.execute("run")