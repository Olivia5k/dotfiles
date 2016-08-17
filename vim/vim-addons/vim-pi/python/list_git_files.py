#!/usr/bin/env python
# vim: fileencoding=utf-8

from tempfile import mkdtemp
from subprocess import check_call, check_output
import os
from shutil import rmtree
import sys


def list_git_files(url, allow_depth=True):
    '''List files contained in remote git repository at given URL

    Currently uses ``git clone`` to a temporary directory. ``allow_depth`` controls whether git will 
    be supplied with ``--depth=1`` argument.

    Returns a list.
    '''
    tmpdir = mkdtemp(suffix='.git')
    try:
        check_call(['git', 'clone'] + (['--depth=1'] if allow_depth else []) +['--', url, tmpdir],
                   stdout=sys.stderr)
        if os.path.isdir(tmpdir):
            return check_output(['git', '--git-dir=' + os.path.join(tmpdir, '.git'),
                                         '--work-tree=' + tmpdir,
                                         'ls-files', '-z']).split('\0')
        else:
            raise IOError('Failed to clone {0}: {1} not found'.format(url, tmpdir))
    finally:
        if os.path.isdir(tmpdir):
            rmtree(tmpdir)

# vim: tw=100 ft=python ts=4 sts=4 sw=4
