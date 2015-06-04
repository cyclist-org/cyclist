import sys
import gdb
from .lib import harvest

class HarvestCommand (gdb.Command):
    """Harvest the heap reachable from the current stack frame, 
    and optionally save in the file specified."""
    
    def __init__(self):
        super(HarvestCommand, self).__init__("harvest",
                                             gdb.COMMAND_USER,
                                             gdb.COMPLETE_FILENAME)
    
    def invoke(self, arg, from_tty):   
        harvest(sys.stdout)

HarvestCommand()