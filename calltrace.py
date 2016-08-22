# MIT License

# Copyright (c) 2016 Rebecca ".bx" Shapiro

# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:

# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

import gdb
import subprocess
import re


def addr2line(elf, addr, verbose=False):
    cmd = "addr2line -e %s 0x%x 2>/dev/null" % (elf, addr)
    if verbose:
        print "executing: '%s'" % cmd
    output = subprocess.check_output(cmd, shell=True)
    if verbose:
        print output
    # format output so it's orgmode friendly, replace ":" with "::"
    return re.sub(":", "::", output.split()[0])


def get_c_function_names(elf, verbose=False):
    cmd = 'readelf -W -s %s 2>/dev/null' % elf
    if verbose:
        print "executing: '%s'" % cmd
    output = subprocess.check_output(cmd, shell=True).split("\n")
    if verbose:
        print "\n".join(output)
    regexp = re.compile("\s+\d+:\s+(?P<addr>[a-fA-f0-9]{4,16})\s+\w*\s+(?P<t>FUNC)\s+\w*\s+\w*\s+\w*\s+(?P<name>[\w_\-\.]+)\s*$")
    results = []
    for l in output:
        m = regexp.search(l)
        if m is not None:
            (addr, name, t) = (m.groupdict()['addr'], m.groupdict()['name'], m.groupdict()['t'])
            if t == "FUNC":
                # print "found FUNC %s at %s" % (name, addr)
                results.append((name, int(addr, 16)))
    return results


class PrintEvent():
    def __init__(self, depth, name, kind, string, log):
        self.depth = depth
        self.name = name
        self.kind = kind
        self.string = string
        self.log = log

    def __call__(self):
        if self.log:
            self.log.write(self.string + "\n")
        else:
            gdb.write(self.string, gdb.STDOUT)


class EntryBreak(gdb.Breakpoint):
    def __init__(self, name, ct):
        gdb.Breakpoint.__init__(self, name, internal=True)
        self.name = name
        self.ct = ct
        self.entered = False

    def stop(self):
        if not self.entered:  # ignore recursive calls
            self.ct.entry_append(self.name)
            try:
                ExitBreak(self.name, self.ct, self)
            except ValueError:
                print "Cannot set FinishBreakpoint for %s" % self.name
                pass
            self.entered = True
        return False


class ExitBreak(gdb.FinishBreakpoint):
    def __init__(self, name, ct, entry):
        gdb.FinishBreakpoint.__init__(self, internal=1)
        self.name = name
        self.ct = ct
        self.entry = entry

    def out_of_scope(self):
        self.entry.entered = False
        print "exit breakpoint for %s out of scope" % self.name
        self.ct.exit_append(self.name, False)

    def stop(self):
        self.ct.exit_append(self.name)
        self.entry.entered = False
        return False


class CallTrace(gdb.Command):
    def __init__(self):
        self.results = []
        self.depth = 0
        self.log = False
        gdb.execute('set python print-stack full')
        gdb.execute('set pagination off')
        gdb.execute('set height unlimited')
        gdb.Command.__init__(self, "calltrace", gdb.COMMAND_DATA)
        self.quiet = False
        self.minimal = False
        self.sourceinfo = False
        self.setup_breakpoints()
        self.setup_exit_handler()

    def setup_exit_handler(self):
        gdb.events.exited.connect(self.finish)

    def entry_append(self, name):
        addr = self.pc()
        outstr = ("*" * (self.depth + 1)) + " > " + name
        if self.sourceinfo:
            outstr += " [[%s]]" % addr2line(self.elf, addr)
        gdb.post_event(PrintEvent(self.depth, name,  "entry", outstr, self.log))
        self.depth += 1

    def pc(self):
        return int(gdb.execute("print/x $pc", to_string=True).split()[2], 16)

    def exit_append(self, name, fake=False):
        addr = self.pc()
        self.depth -= 1
        outstr = ("*" * (self.depth + 1)) + " < " + name
        if not fake:
            if not self.minimal:
                outstr += "@0x%x" % addr
            if self.sourceinfo:
                outstr += " [[%s]]" % addr2line(self.elf, addr)
        else:
            outstr += " (return out of scope)"
        gdb.post_event(PrintEvent(self.depth, name,  "exit", outstr, self.log))

    def finish(self, event):
        try:
            print "Execution finished, exit code %d." % event.exit_code
        except Exception:
            print "Execution finished"
        if self.log:
            print "results written to %s" % self.log.name
            self.log.close()

    def invoke(self, arg, from_tty):
        args = gdb.string_to_argv(arg)
        if len(args) == 1:
            if args[0] == "minimal":
                self.minimal = True
                self.sourceinfo = False
            if args[0] == "nominimal":
                self.minimal = False
            if args[0] == "log":
                self.log = False
            if args[0] == "sourceinfo":
                self.sourceinfo = True
            if args[0] == "nosourceinfo":
                self.sourceinfo = False
        elif len(args) == 2:
            if args[0] == "log":
                print "setting log to %s" % args[1]
                self.log = open(args[1], "w")
        elif len(args) == 0:
            gdb.execute("r")

    def setup_breakpoints(self):
        self.elf = gdb.current_progspace().filename
        print "Searching symbol table for functions, this may take a while..."
        functions = get_c_function_names(self.elf)
        for (name, addr) in functions:
            EntryBreak(name, self)
        print "...done."

ct = CallTrace()
