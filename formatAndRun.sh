#!/bin/bash

for i in {01..23}
do
    echo
    echo "Day $i"
    cd day_$i; brittany --write-mode=inplace part1.hs; brittany --write-mode=inplace part2.hs; cd ..
    cd day_$i; runhaskell part1.hs input; runhaskell part2.hs input; cd ..
done

