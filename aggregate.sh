BASEDIR=$(cd $(dirname $0); pwd)
FILES=$(for fn in $@; do echo $(cd $(dirname $1); echo "$(pwd)/$(basename $1)"); done)
mono -O=all $BASEDIR/Analyze/bin/Release/Analyze.exe $FILES | python $BASEDIR/plot.py