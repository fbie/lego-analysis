for f in *{1,2}-raw.csv; do [[ $(head -1 "$f") =~ "eT;dT;aT;" ]] || echo "$(cat $f | sed 1d)" > $f; done
