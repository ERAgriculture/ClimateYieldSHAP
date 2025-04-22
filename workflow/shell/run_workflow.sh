#!/bin/bash

# Check if exactly two arguments are provided
if [ $# -ne 2 ]; then
    echo "Incorrect number of arguments. Usage: ./run_workflow.sh <experiment_name> <AEZ>"
    exit 1
fi

experiment=$1
AEZ=$2

# Create a log directory if it doesn't exist
mkdir -p logs

# Run the R script with nohup, passing the two arguments
nohup Rscript workflow.R "$experiment" "$AEZ" > "logs/${experiment}_${AEZ}_nohup.out" 2>&1 &

echo "Started experiment $experiment with AEZ $AEZ. Check logs/${experiment}_${AEZ}_nohup.out for output."
