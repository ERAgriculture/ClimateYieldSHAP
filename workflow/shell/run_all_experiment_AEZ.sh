#!/bin/bash

# Name of the file containing experiment names and AEZ values
input_file="experiments_and_aez.txt"

# Check if the input file exists
if [ ! -f "$input_file" ]; then
    echo "Input file $input_file not found!"
    exit 1
fi

# Read the file line by line
while IFS=' ' read -r experiment aez || [[ -n "$experiment" ]]; do
    # Skip empty lines and lines starting with #
    [[ -z "$experiment" || "$experiment" == \#* ]] && continue
    
    # Run the workflow script for each combination
    ./run_workflow.sh "$experiment" "$aez"
    
    # Optional: add a small delay between runs
    sleep 2
done < "$input_file"

echo "All experiments have been started."
