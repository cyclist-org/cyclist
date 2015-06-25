#!/usr/local/archer/bin/gdb -P
import sys
import os.path
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
            while True:
                filename = "{0}{1:04d}.mdl".format(self.filename_base, self.counter.next())
                if not os.path.exists(filename): break
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

exe_str = sys.argv[0].strip()
exe_file = exe_str.split(' ')[0]

# Set the file to be executed
gdb.execute("file " + exe_file)

# Set up all the breakpoints
for i in range(1, len(sys.argv), 3):
    HarvesterBP(sys.argv[i], sys.argv[i+1], sys.argv[i+2])

run_cmd = "run"
if ' ' in exe_str : run_cmd += " " + sys.argv[0].split(' ',1)[1]

# Run the file
gdb.execute(run_cmd)