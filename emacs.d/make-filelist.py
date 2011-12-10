#!/usr/bin/env python

import os, sys

EXCLUDE_PATH = ['lost+found', 'autom4te.cache', 'blib', '_build', '.bzr', '.cdv', 'cover_db',
                'CVS', '_darcs', '~.dep', '~.dot', '.git', '.hg', '~.nib', '.pc', '~.plst',
                'RCS', 'SCCS', '_sgbak', '.svn']

HOME = os.environ['HOME']

scanned_dirs = [HOME, '/opt/local/lib/ruby1.9/']

def scan_dir(dir):
    for root, dirs, files in os.walk(dir):
        for ex in EXCLUDE_PATH:
            if ex in dirs:
                dirs.remove(ex)
        d = root.replace(HOME, '~')
        for dir in dirs:
            print os.path.join(d, dir)
        for file in files:
            if file[-1] != '~':
                print os.path.join(d, file)

for dir in scanned_dirs:
    scan_dir(dir)

