BASEDIR=$(cd $(dirname $0); pwd)
PLOTS=$BASEDIR/plots
mkdir -p $PLOTS
mono -O=all $BASEDIR/Analyze/bin/Release/Analyze.exe $(cd $(dirname $1); echo "$(pwd)/$(basename $1)") | python $BASEDIR/plot.py $PLOTS/$(basename $1)
