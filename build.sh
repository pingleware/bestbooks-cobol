#!/bin/bash

# Check if a filename is provided as an argument
if [ $# -eq 0 ]; then
    echo "Error: Please provide a filename as an argument."
    exit 1
fi

# Extract the filename from the argument
filename="$1"

# Check if the file exists
if [ ! -e "$filename.cob" ]; then
    echo "Error: File '$filename.cob' not found."
    exit 1
fi

# Create the 'dist' directory if it doesn't exist
mkdir -p dist

# Compile the COBOL file
cobc -x "$filename.cob" -o "dist/$filename"

# Check if compilation was successful
if [ $? -eq 0 ]; then
    echo "Compilation successful. Executable saved in 'dist/$filename'."
else
    echo "Error: Compilation failed."
fi
