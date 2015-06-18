#!/usr/local/archer/bin/gdb -P
import sys
import traceback
import itertools
import gdb
from gdb_harvest.lib import harvest

class HarvesterBP(gdb.Breakpoint):

    def __init__(self, spec, var_list, filename_base):
        super(HarvesterBP, self).__init__(spec)
        self.var_list = var_list
        self.filename_base = filename_base
        self.counter = itertools.count()

    def stop(self):
        vars = self.var_list.split(',')
        if self.filename_base == '' :
            harvest(sys.stdout, vars)
        else :
            filename = "{0}{1:04d}.mdl".format(self.filename_base, self.counter.next())
            try:
                with open(filename, 'w') as f:
                    harvest(f, vars)
            except:
                os.remove(filename)
                traceback.print_exc()
        return False

if (len(sys.argv) < 4) or ((len(sys.argv) - 1) % 3 != 0):
    print "usage: " + __file__ + " <file-to-run> [<breakpoint> <vars> <output-file-prefix>]+"
    print "\tpass empty string as <output-file_prefix> to output to stdout"
    sys.exit()

# Set the file to be executed
gdb.execute("file " + sys.argv[0])

# Set up all the breakpoints
for i in range(1, len(sys.argv), 3):
    HarvesterBP(sys.argv[i], sys.argv[i+1], sys.argv[i+2])

# Run the file
gdb.execute("run")