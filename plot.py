#!/usr/bin/python

from collections import defaultdict
import string
import sys
import matplotlib.pyplot as plt

COLORS = ('b', 'g', 'r', 'c', 'm', 'y', 'k')
LINES = ('-', '--', '-.', ':')
MARKERS = ('.', ',', 'o', 'v', '^', '<', '>', 's', '*', '+')

LINECOLORS = list(c + l for l in LINES for c in COLORS)[::-1]
MARKERCOLORS = list(c + m for m in MARKERS for c in COLORS)[::-1]
JUSTCOLORS = list(COLORS)[::-1]

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
        plt.figure(figsize=(16, 9), dpi=92)

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

    def bars(self, cmd):
        data = eval(cmd['data'])
        plt.bar(left=[x for x,y in data],
                height=[y for x,y in data],
                width=float(cmd['width']),
                label=cmd['label'],
                color=JUSTCOLORS.pop())

    def xaxis(self, cmd):
        plt.xlabel(cmd['label'])

    def xlim(self, cmd):
        plt.xlim((float(cmd['xmin']), float(cmd['xmax'])))

    def yaxis(self, cmd):
        plt.ylabel(cmd['label'])

    def ylim(self, cmd):
        plt.ylim((float(cmd['ymin']), float(cmd['ymax'])))

    def subplot(self, cmd):
        self.sp_last = plt.subplot(int(cmd['width']), int(cmd['height']), self.sp_id)
        self.sp_id += 1

    def legend(self, cmd):
        if self.sp_last:
            self.sp_last.legend()
        else:
            plt.legend()

    def done(self, cmd):
        args = sys.argv[1:]
        if len(args) != 0:
            plt.savefig(args[0].split('.')[0] + '.png')
        else:
            plt.show()
        sys.exit(0)

plotter = Plotter()
for l in sys.stdin:
    if l in string.whitespace:
        continue
    line = map(lambda x: x.split('='), [s for s in l.strip().split(';') if s])
    try:
        cmd = superdefaultdict(line)
        eval('plotter.' + cmd['cmd'])(cmd)
    except Exception as e:
        print("Error: %s\nInput:%s", (e, l))
sys.exit(1)
