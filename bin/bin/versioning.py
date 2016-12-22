#!/usr/bin/env python
""" 
VERSIONING.PY

Print information on the current python environment and version
information for the numerical and visualization libraries currently
active.

Author: Daniel Rothenberg <darothen@mit.edu>
Verison: September 1, 2015

"""

from __future__ import print_function
from argparse import ArgumentParser
from importlib import import_module
from subprocess import call

parser = ArgumentParser(description="Print some environment information.")
parser.add_argument('--env', action='store_true',
                    help="Print current conda environment")
parser.add_argument('-p', '--packages', nargs='+',
                    help="Additional packages to query")

DEFAULT_PACKAGES = [ 'IPython', ]

if __name__ == "__main__":

    args = parser.parse_args()

    # If available, add custom packages to default list
    if args.packages:
        DEFAULT_PACKAGES.extend(args.packages)
        DEFAULT_PACKAGES.sort()

    print()
    print("Current Python Environment")
    print("--------------------------")
    print()

    # Print conda environment info
    if args.env:
        call(['conda', 'info'])
        print("--------------------------")
        print()

    print("Currently loaded modules:", end="\n\n")
    for pkg in sorted(DEFAULT_PACKAGES):

        # Force import if not already available
        locals()[pkg] = import_module(pkg)

        # Get version if available
        try: 
            v = locals()[pkg].__version__

            print("{:>15s}: {:<8s}".format(pkg, v))

        except KeyError:
            continue

    print()
