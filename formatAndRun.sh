#!/bin/bash

for i in {01..25}
do
    echo "Day $i"
    PART1=src/Day"$i"Part1.hs
    PART2=src/Day"$i"Part2.hs
    brittany --columns 100 --write-mode=inplace $PART1; brittany --columns 100 --write-mode=inplace $PART2
    runhaskell $PART1 input/input"$i"; runhaskell $PART2 input/input"$i"
    hlint -q $PART1; hlint -q $PART2
    echo
done
