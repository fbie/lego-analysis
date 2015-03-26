BASEDIR=$(cd $(dirname $0); pwd)
FSD=$BASEDIR/packages/FSharp.Data.2.1.1/lib/net40/FSharp.Data.dll
ANALYZE=$BASEDIR/Analyze
BIN=$ANALYZE/bin/Release
mkdir -p $BIN
cp $FSD $BIN
cd $ANALYZE
fsharpc -r $FSD Time.fs Gaze.fs Waves.fs Session.fs Chart.fs Main.fs -o $BIN/Analyze.exe
