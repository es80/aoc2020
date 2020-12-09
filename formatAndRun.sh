#!/bin/bash

for i in {1..9}
do
    echo
    echo "Day $i"
    cd day$i; brittany --write-mode=inplace part1.hs; brittany --write-mode=inplace part2.hs; cd ..
    cd day$i; runhaskell part1.hs input; runhaskell part2.hs input; cd ..
done



