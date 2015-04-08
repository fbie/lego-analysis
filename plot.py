#!/usr/bin/python

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
    print n
    cm = plt.get_cmap('gist_rainbow')
    for i in xrange(n):
        yield cm(1.0 * i / n)

def mkColorMap(n):
    return dict(map(lambda (i, c): (i + 1, c), enumerate(colors(n))))

def mkAbsolute(s, x):
    for i in xrange(len(x)):
        t = x[i]
        x[i] = s
        s += t
    return x

if __name__ == '__main__':
    vals = list(parse())
    cm = mkColorMap(27)
    cl = map(lambda x: cm[int(x[0])], vals)
    x_vals = mkAbsolute(0.0, [v[1] for v in vals])
    y_vals = [v[2] / v[1] for v in vals]
    w_vals = [v[1] for v in vals]
    plt.bar(left=x_vals,
            height=y_vals,
            width=w_vals,
            linewidth=0.0,
            color=cl)
    plt.xlabel('Time (s)')
    plt.show()
