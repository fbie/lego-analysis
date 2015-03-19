for f in $1/*-$1.csv; do if grep -q "Next step;16" $f; then cp $f clean/; fi; done
