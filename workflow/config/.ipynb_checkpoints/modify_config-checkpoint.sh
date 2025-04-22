#!/bin/bash

# Find all .yaml files in the current directory and its subdirectories
find . -type f -name "*.yaml" -exec sed -i \
    -e 's/max\.depth: \[3, 5, 10, 15\]/max.depth: [2, 3, 5, 7]/g' \
    -e 's/min_n: \[50, 75, 100, 125\]/min_n: [25, 40, 65, 100]/g' \
    -e 's/sample\.fraction: \[0\.5, 0\.7, 0\.9\]/sample.fraction: [0.5, 0.6, 0.7, 0.8]/g' \
    -e 's/trees: 500/trees: 300/g' \
    -e 's/mtry: \[3, 5, 7, 9\]/mtry: [3, 5]/g' \
    {} +
