#!/usr/bin/python

# Imports
import sys
import os.path
import re

# Check if argument exists
if len(sys.argv) < 2:
    print('Give me a file! }:C')
    exit(0)

# Get first argument as input file name
input_file_name = str(sys.argv[1])

# Check if file name suffix is '.lhs'
if len(input_file_name) < 4 or input_file_name[-4:] != '.lhs':
     print('I don\'t like this file')
     exit(0)
else:
     output_file_name = input_file_name[:-4] + '.hs'

# Get execution directory
dir_path = os.path.dirname(os.path.realpath(__file__))

# Check if file exists
if not os.path.isfile(input_file_name): # in current dir
    if os.path.isfile(dir_path + '\\' + input_file_name): # in exec dir
        input_file_name = dir_path + '\\' + input_file_name
        output_file_name = dir_path + '\\' + output_file_name
    else:
        print('I can\'t see this shit')
        exit(0)

# Open files
input_file = open(input_file_name, 'r')
output_file = open(output_file_name, 'w')

# Read input file
lines = input_file.readlines()

# Write line by line
for line in lines:
    if len(line) <= 1: # empty line
        output_file.write(line)
    elif line[0] == '>': # code
        output_file.write(line[2:])
    else: # comment
        m = re.search('[0-9a-zA-Z]', line) # search for first alphanumeric symbol
        if m: 
            line = ('').join(list(line)[m.start():])
            output_file.write('-- ' + line)
        else: # if there is no alphanumeric
            output_file.write('\n')
        

print('Nya! :3')

# Close files
input_file.close()
output_file.close()

