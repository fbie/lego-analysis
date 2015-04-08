BASEDIR=$(cd $(dirname $0); pwd)
FILES=$(for fn in $@; do echo $(cd $(dirname $fn); echo "$(pwd)/$(basename $fn)"); done)
mono -O=all $BASEDIR/Analyze/bin/Release/Analyze.exe $FILES #| python $BASEDIR/plot.py
