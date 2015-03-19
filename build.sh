BASEDIR=$(cd $(dirname $0); pwd)
FSD=$BASEDIR/packages/FSharp.Data.2.1.1/lib/net40/FSharp.Data.dll

cd $BASEDIR/Analyze
mkdir -p bin
cp $FSD bin/
fsharpc -r $FSD Time.fs Action.fs Raw.fs Waves.fs Session.fs Chart.fs Main.fs -o bin/Analyze.exe
