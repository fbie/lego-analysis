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

DEFAULT = { 'label': '', 'alpha': 1.0, 'width': 1 }

class superdefaultdict(defaultdict):
    def __init__(self, vals):
        defaultdict.__init__(self, None, vals)

    def __getitem__(self, key):
        val = self.get(key)
        if not val:
            return DEFAULT[key]
        return val

def points(cmd):
    title = cmd["title"]
    data = eval(cmd["data"])
    s = MARKERCOLORS.pop()
    plt.scatter([x for x,y in data], [y for x,y in data], c=s[0], marker=s[1], alpha=0.5, label=title, antialiased=True)

def lines(cmd):
    title = cmd["title"]
    data = eval(cmd["data"])
    plt.plot([x for x,y in data], [y for x,y in data], LINECOLORS.pop(), label=title, alpha=float(cmd['alpha']), linewidth=int(cmd['width']), antialiased=True)

def xaxis(cmd):
    plt.xlabel(cmd["label"])

def yaxis(cmd):
    plt.ylabel(cmd["label"])


for l in fileinput.input():
    if l in (' ', '\n'):
        continue
    line = map(lambda x: x.split('='), [s for s in l.strip().split(';') if s])
    try:
        cmd = superdefaultdict(line)
        eval(cmd["cmd"])(cmd)
    except Exception as e:
        print e

plt.legend()
plt.show()
