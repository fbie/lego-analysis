BASEDIR=$(cd $(dirname $0); pwd)
mono -O=all $BASEDIR/Analyze/bin/Analyze.exe $(cd $(dirname $1); echo "$(pwd)/$(basename $1)") | python $BASEDIR/plot.py plots/$(basename $1)
