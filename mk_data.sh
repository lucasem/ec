#!/bin/bash
# produces data (data.tsv) and learning curve plots (results.eps)
# REQUIRES jq (https://stedolan.github.io/jq)

cd $(dirname ${BASH_SOURCE[0]})

EC=./ec
INPUT=flashfill.json
OUTPUT=data.tsv

if [ ! -f $EC ]
then >&2 echo "please build `pwd`/ec before use"
  exit 1
fi

for smoothing in 0.6 1.0 1.4
do for lambda in 0.5 1.001 1.5
do for frontier_size in 2000 5000 10000 20000
do for it in {1..6}
  do echo "ec -it $it -frontier-size $frontier_size -lambda $lambda -smoothing $smoothing"
    $EC -it $it -frontier-size $frontier_size -lambda $lambda -smoothing $smoothing \
      $INPUT | jq -r '"'"$frontier_size\t$it\t$lambda\t$smoothing\t"'\(.hit_rate)"' >> $OUTPUT
done
done
done
done

python plot.py data.tsv
