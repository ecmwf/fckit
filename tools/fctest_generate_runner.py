# (C) Copyright 2013 ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

from argparse import ArgumentParser
import re

parser = ArgumentParser()
parser.add_argument('-i', '--input', type=str, help='input source file')
parser.add_argument('-o', '--output', type=str, help='output source file')
args = parser.parse_args()

test_names = []
testsuite = "tests"
init = False
finalize = False
source_in = args.input
source_out = args.output

with open(source_in,'r') as file:

    for line in file.readlines():
        stripped = line.strip()

        m = re.search(r'^\s*TESTSUITE\s*\(\s*([^\)]+)\s*\)',stripped)
        if m:
            testsuite = m.group(1).strip()

        m = re.search(r'^\s*TESTSUITE_WITH_FIXTURE\s*\(([^,]+),([^\)]+)\)',stripped)
        if m:
            testsuite = m.group(1).strip()

        m = re.search(r'^\s*TEST\s*\(\s*([^\)]+)\s*\)',stripped)
        if m:
            test_names.append( m.group(1).strip() )

        if( re.search(r'^\s*TESTSUITE_INIT',stripped) ):
            init = True
        if( re.search(r'^\s*TESTSUITE_FINALI[SZ]E',stripped) ):
            finalize = True

source_file_pieces = source_in.split('/')

runner =  '#include "'+source_in+'"\n'
runner += 'program run_'+testsuite+'\n'
runner += 'use '+testsuite+'\n'
runner += 'implicit none\n'
runner += 'source_file=""\n'
for piece in source_file_pieces[:-1]:
    runner += 'source_file=trim(source_file)//"'+piece+'/"\n'
runner += 'source_file=trim(source_file)//"'+source_file_pieces[-1]+'"\n'
if( init ): runner += 'call testsuite_init \n'
for test in test_names:
    runner += 'call '+test+'\n'
if( finalize ): runner += 'call testsuite_finalize \n'
runner += 'if( exit_status /= 0 ) STOP 1 \n'
runner += 'end program\n'

with open(source_out,'w') as file:
    file.write(runner)
