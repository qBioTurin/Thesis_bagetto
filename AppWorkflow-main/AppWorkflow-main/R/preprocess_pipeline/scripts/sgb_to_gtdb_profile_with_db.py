#!/usr/bin/env python
__author__ = 'Aitor Blanco (aitor.blancomiguez@unitn.it) feat franky2204(fpantini1997@gmail.com)'
__version__ = 'Given as 3rd input'
__date__    = '10 Nov 2025'

import time
import argparse as ap
import sys

def info(msg):
    print(f"[INFO] {msg}")

def error(msg, exit=False):
    print(f"[ERROR] {msg}")
    if exit:
        sys.exit(1)

# parameters reading
def read_params():
    p = ap.ArgumentParser(
        description="", formatter_class=ap.ArgumentDefaultsHelpFormatter)
    p.add_argument('-i', '--input', type=str, required=True,
                   help="The input profile")
    p.add_argument('-o', '--output', type=str, required=True,
                   help="The output profile")
    p.add_argument('-db', '--db_version', type=str, required=True,
                   help="The GTDB mapping file to use")
    p.add_argument('-m', '--file-merged', action='store_true',
                   help="If specified, the input profile is considered as merged")
    return p.parse_args()

# Controllo parametri
def check_params(args):
    if not args.input:
        error('-i (or --input) must be specified', exit=True)
    if not args.output:
        error('-o (or --output) must be specified', exit=True)
    if not args.db_version:
        error('-db (or --db_version) must be specified', exit=True)

# Profile conversion
def get_gtdb_profile(mpa_profile, gtdb_profile, db_version, file_merged=False):
    tax_levels = ['d', 'p', 'c', 'o', 'f', 'g', 's']
    sgb2gtdb = dict()

    # Load the mapping
    with open(db_version, 'r') as read_file:
        for line in read_file:
            line = line.strip().split('\t')
            sgb2gtdb[line[0]] = line[1]

    abundances = {x: dict() for x in tax_levels}

    with open(gtdb_profile, 'w') as wf, open(mpa_profile, 'r') as rf:
        for line in rf:
            if (not file_merged):
                if line.startswith('#mpa_'):
                    wf.write(line)
                    wf.write('#clade_name\trelative_abundance\n')
                elif line.startswith('UNCLASSIFIED'):
                    fields = line.strip().split('\t')
                    unclassified = float(fields[2]) if len(fields) > 2 else float(fields[1])
                    wf.write('UNCLASSIFIED\t{}\n'.format(unclassified))
                elif 't__SGB' in line:
                    fields = line.strip().split('\t')
                    sgb_id = fields[0].split('|')[-1][3:].replace("_group","")
                    if sgb_id not in sgb2gtdb:
                        continue
                    gtdb_tax = sgb2gtdb[sgb_id]
                    if gtdb_tax not in abundances['s']:
                        abundances['s'][gtdb_tax] = 0
                    abundances['s'][gtdb_tax] += float(fields[2])
            else:
                if line.startswith('#mpa_'):
                    wf.write(line)
                elif line.startswith('clade_name'):
                    wf.write(line)
                elif line.startswith('UNCLASSIFIED'):
                    fields = line.strip().split('\t')
                    unclassified_values = '\t'.join(fields[1:])
                    wf.write('UNCLASSIFIED\t{}\n'.format(unclassified_values))
                elif 't__SGB' in line:
                    fields = line.strip().split('\t')
                    sgb_id = fields[0].split('|')[-1][3:].replace("_group","")
                    if sgb_id not in sgb2gtdb:
                        continue
                    gtdb_tax = sgb2gtdb[sgb_id]
                    abundance_values = '\t'.join(fields[1:])
                    wf.write('{}\t{}\n'.format(gtdb_tax, abundance_values))
                

    tax_levels.reverse()
    for tax_level in tax_levels[:-1]:
        for tax in abundances[tax_level]:
            new_tax = tax.replace(';{}'.format(tax.split(';')[-1]), '')
            new_level = tax_levels[tax_levels.index(tax_level)+1]
            if new_tax not in abundances[new_level]:
                if file_merged:
                    abundances[new_level][new_tax] = [0.0] * len(abundances[tax_level][tax])
                else:
                    abundances[new_level][new_tax] = 0
            if file_merged:
                for i in range(len(abundances[tax_level][tax])):
                    abundances[new_level][new_tax][i] += abundances[tax_level][tax][i]
            else:
                abundances[new_level][new_tax] += abundances[tax_level][tax]
    tax_levels.reverse()

    with open(gtdb_profile, 'a') as wf:
        for tax_level in tax_levels:
            for tax in abundances[tax_level]:
                if file_merged:
                    wf.write('{}\t{}\n'.format(tax, '\t'.join(map(str, abundances[tax_level][tax]))))
                else:
                    wf.write('{}\t{}\n'.format(tax, abundances[tax_level][tax]))

def main():
    t0 = time.time()
    args = read_params()
    info("Start execution")
    check_params(args)
    get_gtdb_profile(args.input, args.output, args.db_version, args.file_merged)
    exec_time = time.time() - t0
    info("Finish execution ({} seconds)".format(round(exec_time, 2)))

if __name__ == "__main__":
    main()
