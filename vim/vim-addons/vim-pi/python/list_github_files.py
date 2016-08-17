# vim:fileencoding=utf-8

from github import Github, GithubException
import logging


logger = logging.getLogger('list_github_files')


class GithubLazy(object):
    '''Proxy to Github() class with delayed initialization'''
    __slots__ = ('args', 'kwargs', 'gh')

    def __init__(self, *args, **kwargs):
        self.args = args
        self.kwargs = kwargs

    def __getattr__(self, attr):
        if attr == 'gh':
            self.gh = Github(*self.args, **self.kwargs)
            return self.gh
        else:
            return getattr(self.gh, attr)

def _list_github_files(repo, dir=None, attempt=0):
    try:
        for f in repo.get_dir_contents(dir or '/'):
            name = (dir + '/' + f.name if dir else f.name)
            if f.type == 'file':
                yield name
            elif f.type == 'dir':
                for subf in _list_github_files(repo, name):
                    yield subf
    except GithubException as e:
        if 500 <= e.status:
            if attempt < MAX_ATTEMPTS:
                logger.error('>>> Received exception, retrying: %s' % repr(e))
                for fname in _list_github_files(repo, dir, attempt + 1):
                    yield fname
            else:
                raise
        else:
            raise


gh = None

def init_gh(user, password):
    '''Initialize global Github object

    Must be called before any other functions in this module.
    '''
    global gh
    gh = GithubLazy(user, password)


def list_github_files(repo_path):
    '''List files in github repository

    Returns a generator that lists files.
    '''
    return _list_github_files(gh.get_repo(repo_path))


def list_gist_files(name):
    '''List files in github gist repository

    Returns a list.
    '''
    return gh.get_gist(name).files
