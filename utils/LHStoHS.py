#!/usr/bin/python

import sys
import os.path
import re

if len(sys.argv) < 2:
    print('Give me a file! }:C')
    exit(0)

input_file_name = str(sys.argv[1])
if len(input_file_name) < 4 or input_file_name[-4:] != '.lhs':
     print('I don\'t like this file')
else:
     output_file_name = input_file_name[:-4] + '.hs'

dir_path = os.path.dirname(os.path.realpath(__file__))
if not os.path.isfile(input_file_name):
    if os.path.isfile(dir_path + '\\' + input_file_name):
        input_file_name = dir_path + '\\' + input_file_name
        output_file_name = dir_path + '\\' + output_file_name
    else:
        print('I can\'t see this shit')
        exit(0)

input_file = open(input_file_name, 'r')
output_file = open(output_file_name, 'w')

lines = input_file.readlines()

for line in lines:
    if len(line) <= 1:
        output_file.write(line)
    elif line[0] == '>':
        output_file.write(line[2:])
    else:
        m = re.search('[0-9a-zA-Z]', line)
        if m:
            line = ('').join(list(line)[m.start():])
            output_file.write('-- ' + line)
        else:
            output_file.write('\n')
        

print('Nya! :3')

input_file.close()
output_file.close()

