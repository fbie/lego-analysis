#!/usr/bin/python

from collections import defaultdict
import string
import sys
import matplotlib.pyplot as plt

def parse():
    for l in sys.stdin:
        if not (l in string.whitespace or l.startswith('#')):
            try:
                vals = map(float, l.split(';'))
                if not vals[0] == 0.0:
                    yield vals
            except Exception as e:
                print e
                continue

def colors(n):
    cm = plt.get_cmap('gist_rainbow')
    for i in xrange(n):
        yield cm(1.0 * float(i) / float(n))

def mkColorMap(n):
    c = colors(n)
    return defaultdict(lambda: c.next())

def undiff(s, x):
    for i in xrange(len(x)):
        t = x[i]
        x[i] = s
        s += t
    return x

if __name__ == '__main__':
    vals = list(parse())
    cm = mkColorMap(27)
    cl = map(lambda x: cm[x[0]], vals)
    x_vals = undiff(0.0, [v[1] for v in vals])
    y_vals = [v[2] / v[1] for v in vals]
    w_vals = [v[1] for v in vals]
    plt.bar(left=x_vals,
            height=y_vals,
            width=w_vals,
            linewidth=0.0,
            color=cl)
    plt.xlabel('Time (s)')
    plt.xlim((0.0, x_vals[-1]))
    plt.ylabel('Attention (s) / Duration (s)')
    plt.show()
