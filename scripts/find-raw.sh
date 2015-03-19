ls clean | grep -oe 'gibi-session-2015-\([0-9]\{2\}-\)\{5\}[0-9]\{6\}-' | xargs -I '%' cp /'%'-raw.csv ../clean
