#!/usr/bin/env python
# vim: fileencoding=utf-8

from __future__ import unicode_literals, division
from xml import sax
from subprocess import check_call, Popen, PIPE
from shutil import rmtree
from tempfile import mkdtemp
import sys


class MercurialRevision(object):
    '''Data class holding revision information

    Contains the following keys:
        ``rev``: revision number
        ``hex``: revision hexadecimal string

        ``tags``, ``bookmarks``: tags and bookmarks that reference this revision
        ``branch``: branch property of the revision

        ``parents``: list of MercurialRevision objects with revision parents
        ``children``: like above, but with revision children

        ``added``, ``removed``, ``modified``:
            set of files that were added, removed or modified in this revision respectively

        ``copies``: dictionary mapping source of the copies to destination

        ``files``: a set of all files present in given revision
    '''
    __slots__ = ('rev', 'hex',
                 'tags', 'bookmarks', 'branch',
                 'parents', 'children',
                 'added', 'removed', 'modified',
                 'copies',
                 'files',)

    def __init__(self, rev, hex):
        self.rev = rev
        self.hex = hex
        self.parents = []
        self.children = []
        self.added = set()
        self.removed = set()
        self.modified = set()
        self.copies = {}
        self.tags = set()
        self.bookmarks = set()
        self.branch = None
        self.files = set()

    def __str__(self):
        return '<Revision {rev}:{hex}>'.format(hex=self.hex, rev=self.rev)

    def __repr__(self):
        return '{0}({rev!r}, {hex!r})'.format(self.__class__.__name__, hex=self.hex, rev=self.rev)

    def __hash__(self):
        return int(self.hex, 16)


class MercurialHandler(sax.handler.ContentHandler):
    '''Class for parsing “hg log --style xml” output

    To get parsing result you should run handler.export_result() after you are done.
    '''
    def startDocument(self):
        self.curpath = []
        self.currev = None
        nullrev = MercurialRevision(-1, '0' * 40)
        self.revisions_rev = {nullrev.rev : nullrev}
        self.revisions_hex = {nullrev.hex : nullrev}
        self.tags = {}
        self.bookmarks = {}
        self.characters_fun = None
        self.last_data = None

    def add_tag(self, tag):
        self.currev.tags.add(tag)
        self.tags[tag] = self.currev

    def add_bookmark(self, bookmark):
        self.currev.bookmarks.add(bookmark)
        self.bookmarks[bookmark] = self.currev

    def characters(self, data):
        if self.characters_fun:
            if not self.last_data:
                self.last_data = data
            else:
                self.last_data += data

    def startElement(self, name, attributes):
        if name == 'log':
            assert not self.curpath
            assert not self.currev
        elif name == 'logentry':
            assert self.curpath == ['log']
            assert not self.currev
            self.currev = MercurialRevision(int(attributes['revision']), attributes['node'])
        else:
            assert self.currev
            if name == 'tag':
                assert self.curpath[-1] == 'logentry'
                self.characters_fun = self.add_tag
            elif name == 'bookmark':
                assert self.curpath[-1] == 'logentry'
                self.characters_fun = self.add_bookmark
            elif name == 'parent':
                assert self.curpath[-1] == 'logentry'
                self.currev.parents.append(self.revisions_hex[attributes['node']])
            elif name == 'branch':
                assert self.curpath[-1] == 'logentry'
                self.characters_fun = lambda branch: self.currev.__setattr__('branch', branch)
            elif name == 'path':
                assert self.curpath[-1] == 'paths'
                if attributes['action'] == 'M':
                    self.characters_fun = self.currev.modified.add
                elif attributes['action'] == 'A':
                    self.characters_fun = self.currev.added.add
                elif attributes['action'] == 'R':
                    self.characters_fun = self.currev.removed.add
            elif name == 'copy':
                assert self.curpath[-1] == 'copies'
                self.characters_fun = (lambda destination, source=attributes['source']:
                        self.currev.copies.__setitem__(source, destination))
        self.curpath.append(name)

    def endElement(self, name):
        assert self.curpath or self.curpath[-1] == ['log']
        assert self.curpath[-1] == name
        if name == 'logentry':
            if not self.currev.parents:
                self.currev.parents.append(self.revisions_rev[self.currev.rev - 1])
            for parent in self.currev.parents:
                parent.children.append(self.currev)
            self.revisions_hex[self.currev.hex] = self.currev
            self.revisions_rev[self.currev.rev] = self.currev
            self.currev = None
        if self.last_data is None:
            if self.characters_fun:
                self.characters_fun('')
        else:
            assert self.characters_fun
            self.characters_fun(self.last_data)
            self.characters_fun = None
            self.last_data = None
        self.curpath.pop()

    def export_result(self):
        '''Export parsing result in a dictionary

        Returns dictionary with the following key:
            ``heads``: set of heads (revisions that do not have any children with the same branch as 
                       itself)
            ``tips``: set of tips (revisions that have no children)
            ``tags``: dictionary mapping tag names to MercurialRevision objects
            ``bookmarks``: same as above, but for bookmarks
            ``revisions_hex``: dictionary mapping revision hexadecimal identifiers to 
                               MercurialRevision objects
            ``revisions_rev``: like above, but maps revision numbers
            ``root``: special revision that is the root of all other revisions
        '''
        heads = {revision for revision in self.revisions_hex.values()
                 if not revision.children
                    or all(child.branch != revision.branch for child in revision.children)}
        # heads contains the same revisions as `hg heads --closed`
        tips = {head for head in heads if not head.children}
        return {
            'heads': heads,
            'tips': tips,
            'tags': self.tags,
            'bookmarks': self.bookmarks,
            'revisions_hex': self.revisions_hex,
            'revisions_rev': self.revisions_rev,
            'root': self.revisions_rev[-1],
        }

class MercurialRemoteParser(object):
    '''Class for with statement for getting revisions of remote repository

    When initialized creates temporary directory and initializes an empty mercurial repository 
    there. On __exit__ this directory is deleted.

    Empty mercurial repository is used because you can run ``hg incoming {url}`` for any URL and 
    this will work listing all commits remote repository at given URL has.
    '''
    __slots__ = ('parser', 'handler', 'tmpdir')

    def __init__(self, tmpdir=None):
        self.parser = sax.make_parser()
        self.handler = MercurialHandler()
        self.parser.setContentHandler(self.handler)
        self.tmpdir = tmpdir or mkdtemp(suffix='.hg')
        self.init_tmpdir()

    def init_tmpdir(self):
        '''Create an empty mercurial repository in self.tmpdir

        If directory does not exist it will be created by “hg init”. stdout of “hg init” is 
        connected to the current stderr.
        '''
        check_call(['hg', 'init', self.tmpdir], stdout=sys.stderr)

    def delete_tmpdir(self):
        '''Remove temporary directory'''
        if self.tmpdir and rmtree:
            rmtree(self.tmpdir)
            self.tmpdir = None

    __del__ = delete_tmpdir

    def __enter__(self):
        return self

    def __exit__(self, *args, **kwargs):
        self.delete_tmpdir()

    @staticmethod
    def generate_files(parsing_result):
        '''Populates .files attributes of MercurialRevision instanses

        Accepts dictionary returned by MercurialHandler.export_result and operates on all revisions 
        it (i.e. parsing_result['revisions_hex']) references.
        '''
        toprocess = [parsing_result['root']]
        processed = set()
        while toprocess:
            revision = toprocess.pop(0)
            if revision.parents:
                # Inherit files from the first parent
                assert not revision.files
                if revision.parents[0] not in processed:
                    assert toprocess
                    toprocess.append(revision)
                    continue
                revision.files.update(revision.parents[0].files)
                # Then apply delta found in log
                assert not (revision.files & revision.added)
                revision.files.update(revision.added)
                assert revision.files >= revision.removed
                revision.files -= revision.removed
                assert revision.files >= revision.modified, (
                        'Expected to find the following files: ' + ','.join(
                            repr(file) for file in revision.modified if not file in revision.files))
            processed.add(revision)
            toprocess.extend(child for child in revision.children
                             if not child in processed and not child in toprocess)
        assert set(parsing_result['revisions_rev'].values()) == processed
        return parsing_result

    def parse_url(self, url, rev_name=None):
        '''Return description of all revisions repository at given URL contains

        Before return parsing result is altered for MercurialRevision instanses to have proper 
        contents in their .files attributes.
        '''
        p = Popen(['hg', '--repository', self.tmpdir,
                         'incoming', '--style', 'xml', '--verbose', url,
                  ] + (['--rev', rev_name] if rev_name else []),
                  stdout=PIPE)
        p.stdout.readline()  # Skip “comparing with {url}” header
        self.parser.parse(p.stdout)
        parsing_result = self.handler.export_result()
        self.generate_files(parsing_result)
        return parsing_result


if __name__ == '__main__':
    import sys

    def print_files(revision):
        for file in revision.files:
            print file

    remote_url = sys.argv[1]
    rev_name = sys.argv[2]

    with MercurialRemoteParser() as remote_parser:
        parsing_result = remote_parser.parse_url(remote_url, rev_name=rev_name)
        assert len(parsing_result['tips']) == 1, 'Found more then one head'
        print_files(next(iter(parsing_result['tips'])))

# vim: tw=100 ft=python ts=4 sts=4 sw=4
