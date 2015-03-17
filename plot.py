#!/usr/bin/python

from collections import defaultdict
import fileinput
import sys
import matplotlib.pyplot as plt

COLORS = ('b', 'g', 'r', 'c', 'm', 'y', 'k')
LINES = ('-', '--', '-.', ':')
MARKERS = ('.', ',', 'o', 'v', '^', '<', '>', 's', '*', '+')

LINECOLORS = list(c + l for l in LINES for c in COLORS)[::-1]
MARKERCOLORS = list(c + m for m in MARKERS for c in COLORS)[::-1]

DEFAULT = { 'label': None, 'alpha': 1.0, 'width': 1, 'height': 1 }

class superdefaultdict(defaultdict):
    def __init__(self, vals):
        defaultdict.__init__(self, None, vals)

    def __getitem__(self, key):
        val = self.get(key)
        if not val:
            return DEFAULT[key]
        return val

class Plotter:
    def __init__(self):
        self.sp_last = None
        self.sp_id = 1

    def points(self, cmd):
        data = eval(cmd['data'])
        s = MARKERCOLORS.pop()
        plt.scatter([x for x,y in data],
                    [y for x,y in data],
                    c=s[0], marker=s[1],
                    alpha=0.5,
                    label=cmd['label'],
                    antialiased=True)

    def lines(self, cmd):
        data = eval(cmd['data'])
        plt.plot([x for x,y in data],
                 [y for x,y in data],
                 LINECOLORS.pop(),
                 label=cmd['label'],
                 alpha=float(cmd['alpha']),
                 linewidth=int(cmd['width']),
                 antialiased=True)

    def xaxis(self, cmd):
        plt.xlabel(cmd['label'])

    def yaxis(self, cmd):
        plt.ylabel(cmd['label'])

    def subplot(self, cmd):
        self.sp_last = plt.subplot(int(cmd['width']), int(cmd['height']), self.sp_id)
        print self.sp_last == None
        self.sp_id += 1

    def legend(self, cmd):
        if self.sp_last:
            print 'self.sp_last.legend()'
            self.sp_last.legend()
        else:
            print 'plt.legend()'
            plt.legend()

plotter = Plotter()
for l in fileinput.input():
    if l in (' ', '\n'):
        continue
    line = map(lambda x: x.split('='), [s for s in l.strip().split(';') if s])
    try:
        cmd = superdefaultdict(line)
        eval('plotter.' + cmd['cmd'])(cmd)
    except Exception as e:
        print("Error: %s", e)
plt.show()
