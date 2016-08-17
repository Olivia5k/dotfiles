#!/usr/bin/env python
# vim: fileencoding=utf-8

from subvertpy.ra import RemoteAccess, DIRENT_KIND
from subvertpy import NODE_DIR

class NoFilesError(Exception):
    pass

def _list_svn_files(conn, path='', only_trunk=True):
    dirents, frev, props = conn.get_dir(path, -1, DIRENT_KIND)
    if not path and only_trunk and 'trunk' in dirents:
        dirents = {'trunk' : dirents['trunk']}
    for fname, props in dirents.items():
        new_path = (path + '/' if path else '') + fname
        contains = False
        if props['kind'] == NODE_DIR:
            for full_fname in _list_svn_files(conn, new_path):
                yield full_fname
        else:
            yield new_path

def list_svn_files(url, only_trunk=True):
    '''List files in remote subversion repository

    Returns either an empty list or a generator.

    If only_trunk is True, then if trunk/ directory is present at the top level, only it is used.
    '''
    conn = RemoteAccess(url)
    try:
        return _list_svn_files(conn, only_trunk=only_trunk)
    except NoFilesError:
        return []

# vim: tw=100 ft=python ts=4 sts=4 sw=4
