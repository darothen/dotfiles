#!/usr/bin/env python
"""
REDUCE_BIB.PY

Given a bibtex archive file and a Markdown manuscript, generate
a reduced bibtex file which only contains the citations for the papers
referenced in the manuscript. Currently, assumes that citations in the
manuscript are prefixed with an "@" sign (e.g. citeproc or natib
format).

Author: Daniel Rothenberg
Version: February 17, 2016

"""
from __future__ import print_function

import codecs
import copy
import os
import re

import bibtexparser
from bibtexparser.bparser import BibTexParser
# from bibtexparser.customization import convert_to_unicode

from argparse import ArgumentParser, RawDescriptionHelpFormatter
parser = ArgumentParser(description=__doc__,
                        formatter_class=RawDescriptionHelpFormatter)

parser.add_argument("bibtex_archive", type=str,
                    help="name of bibtex archive")
parser.add_argument("manuscript", type=str,  nargs="+",
                    metavar="<first.md, ..., last.md>",
                    help="names of manuscript files")
parser.add_argument("--output", "-o", type=str,
                    help="name of output file with bibtex subset")


def split_filename(path_to_file):
    """ Split a filename into its root path, its basename, and
    its extension """
    path, ext = os.path.splitext(path_to_file)
    path, basename = os.path.split(path)
    return path, basename, ext

# Regex for finding MD citekeys
PATTERN = re.compile(r"""
    \W                       # non-alphanumeric character before "@"
    @{1}                     # entries begining with "@"
    (?P<citekey>(\w|\-|\_)*) # any combination of alpha-numeric, "-", or "_"
""", re.VERBOSE)

if __name__ == "__main__":

    args = parser.parse_args()

    arch_fn = args.bibtex_archive
    if args.output:
        output_fn = args.output
    else:
        _, m_basename, _ = split_filename(args.manuscript[0])
        output_fn = "%s.bib" % m_basename

    #######################################################
    # Read the bibtex archive
    print("Analyzing archive in %s" % arch_fn)

    bp = BibTexParser()
    # bp.customization = convert_to_unicode
    with open(arch_fn, 'r') as bibfile:
        bib_db = bibtexparser.load(bibfile, parser=bp)

    # Extract the entries in the bibtex archive and their keys
    entries = bib_db.get_entry_dict()
    entry_keys = entries.keys()

    # Create a set of bibtex keys referenced in the manuscript
    manu_keys = set()

    #######################################################
    # Read the manuscripts
    print("Analyzing manuscript files... ")
    for i, manu_fn in enumerate(args.manuscript, start=1):
        print("{:02d}) {:s}".format(i, manu_fn))

        with open(manu_fn, 'rb') as manufile:
            lines = manufile.readlines()
        lines = [l for l in lines if l != "\n"]

        print("   Found keys: ", end=' ')
        for line in lines:
            line = str(line)
            if "@" not in line:
                continue
            matches = re.findall(PATTERN, line)

            if matches:
                line_keys, _ = zip(*matches)
                line_keys = set(line_keys)

                new_keys = line_keys.difference(manu_keys)
                for key in new_keys:
                    manu_keys.add(key)
                    print(key, end=' ')
        print()  # Close "found keys" print loop

    #######################################################
    # Assemble a bibtex subset using the found keys

    # First check if any keys from the manuscript are MISSING
    # from the bibtex archive!
    missing_keys = manu_keys.difference(entry_keys)
    if missing_keys:
        print("WARNING: The following keys could not be found in %s" % arch_fn)
        for i, k in enumerate(missing_keys):
            manu_keys.discard(k)
            print(i, k)

    # Else, we can go ahead and assemble the subset.
    entries_subset = {
        key: entries.get(key, None) for key in manu_keys
    }

    entries = [v for v in entries_subset.values()]

    # Modify a copy of the bib_db
    bib_subset = copy.copy(bib_db)
    bib_subset.entries = entries
    bib_subset._entry_dict = entries_subset

    # Write to disk; we write a UTF-8 encoded file for unicode support
    print("\nWriting %s" % output_fn)
    with codecs.open(output_fn, 'wb', 'utf-8') as subset_file:
        bibtexparser.dump(bib_subset, subset_file)
    print("...done.")
