#!/usr/bin/env python

import fileinput
import re

re_char = '^[A-Z][A-Z- ]+$'
re_move = '^.* - .*$'

curr_char = ''

for line in fileinput.input():
    line = line.strip()
    if re.match(re_char, line):
        curr_char = line.title()
    elif re.match(re_move, line):
        # Split by hyphen
        # Assemble name (character + move)
        # Keep the comma as splitter
        (name, sep, rule) = line.partition(' - ')
        name = name.strip() + ' (' + curr_char + ')'
        print '%s ::= %s' % (name.strip(), rule)
