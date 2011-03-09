#!/usr/bin/env python
# coding=utf-8

import os
import re
import sys

import subprocess as sub

VERBOSE = True

def debug(string):
    if VERBOSE:
        print(string)

class Windowlist(object):
    def __init__(self):
        self.windows = []

        cmd = ["/usr/bin/tmux", "list-win"]
        out = sub.Popen(cmd, stdout=sub.PIPE).communicate()[0].decode('utf-8')

        for line in out.split('\n'):
            if not line:
                continue

            self.windows.append(Window(line))

    def arrange(self):
        for window in self.windows:
            #debug('%s: %s' % (window.index, window.active))
            window.newindex = self.windows.index(window)
            window.move()

class Window(object):
    def __init__(self, line):
        self.line = line
        self.index = int(re.search(r'^(\d+):', line).group(1))
        self.active = bool(re.search(r'\(active\)$', line))
        self.newindex = None

    def move(self, goto=False):
        if not self.newindex:
            debug("no new index")
            return False

        elif self.newindex == self.index:
            debug("same index")
            return False

        cmd = ['/usr/bin/tmux', 'move-window',]

        if not goto:
            cmd.append('-d')

        cmd.append('-s')
        cmd.append(':%s' % self.index)
        cmd.append('-t')
        cmd.append(':%s' % self.newindex)

        debug(cmd)
        sub.Popen(cmd, stdout=sub.PIPE).communicate()
        return True

if __name__ == "__main__":
    windowlist = Windowlist()

    if sys.argv[1] == "--arrange":
        windowlist.arrange()
