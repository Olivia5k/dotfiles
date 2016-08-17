# vim:fileencoding=utf-8

from __future__ import unicode_literals, division, print_function

import httplib
import logging
import re

from vimorg import get_file_list, compare_file_lists

import list_hg_files as lshg
import list_git_files as lsgit
import list_svn_files as lssvn
import list_github_files as lsgh

logger = logging.getLogger('vomatch')


MAX_ATTEMPTS = 5


github_url              = re.compile(r'github\.com/([0-9a-zA-Z\-_]+)/([0-9a-zA-Z\-_.]+)(?:\.git)?')
vundle_github_url       = re.compile('\\b(?:Neo)?Bundle\\b\\s*[\'"]([0-9a-zA-Z\-_]+)/([0-9a-zA-Z\-_.]+)(?:.git)?[\'"]')
vundle_github_url_2     = re.compile('\\b[Bb]undle\\b(?:\s+\\w+)?\s+`?[\'"]?([0-9a-zA-Z\-_]+)/([0-9a-zA-Z\-_.]+)(?:.git)?[\'"]?')
gist_url                = re.compile(r'gist\.github\.com/(\d+)')
bitbucket_mercurial_url = re.compile(r'\bhg\b[^\n]*bitbucket\.org/([0-9a-zA-Z_]+)/([0-9a-zA-Z\-_.]+)')
bitbucket_git_url       = re.compile(r'\bgit\b[^\n]*bitbucket\.org/([0-9a-zA-Z_]+)/([0-9a-zA-Z\-_.]+)|bitbucket\.org/([0-9a-zA-Z_.]+)/([0-9a-zA-Z\-_.]+)\.git')
bitbucket_noscm_url     = re.compile(r'bitbucket\.org/([0-9a-zA-Z_]+)/([0-9a-zA-Z\-_.]+)')
bitbucket_site_url      = re.compile(r'([0-9a-zA-Z_]+)\.bitbucket\.org/([0-9a-zA-Z\-_.]+)')
codegoogle_url          = re.compile(r'code\.google\.com/([pr])/([^/\s]+)')
mercurial_url           = re.compile(r'hg\s+clone\s+(\S+)')
mercurial_url_2         = re.compile(r'\b(?:(?i)hg|mercurial)\b.*\b(\w+(?:\+\w+)*://\S+)')
git_url                 = re.compile(r'git\s+clone\s+(\S+)')
subversion_url          = re.compile(r'svn\s+(?:checkout|co)\s+(\S+)')


class NotLoggedError(Exception):
    pass


class cached_property(object):
    '''cached_property decorator

    Found somewhere on the internet. Uses __dict__ to cache properties into it.
    '''
    def __init__(self, func):
        self.func = func

    def __get__(self, instance, cls=None):
        result = instance.__dict__[self.func.__name__] = self.func(instance)
        return result


def novimext(s):
    '''Strip .vim extension from the given string'''
    if s.endswith('.vim'):
        return s[:-4]
    else:
        return s


def novimprefix(s):
    '''Strip vim- prefix from the given string'''
    if s.startswith('vim-'):
        return s[4:]
    else:
        return s


class Match(object):
    '''Base class for all matches'''
    def __init__(self, match, voinfo):
        self.match = match
        self.voinfo = voinfo

    @cached_property
    def name_key(self):
        name = self.name
        # TODO Also compare author names
        try:
            voname = self.voinfo['script_name']
        except KeyError:
            return 0
        if not name or not voname:
            return 0
        elif name == voname:
            return 10
        elif name.lower() == voname.lower():
            return 9
        elif novimext(name) == novimext(voname):
            return 9
        elif novimext(name.lower()) == novimext(voname.lower()):
            return 8
        elif novimprefix(name) == novimprefix(voname):
            return 7
        elif novimprefix(name.lower()) == novimprefix(voname.lower()):
            return 6
        else:
            name = novimext(novimprefix(name.lower()))
            voname = novimext(novimprefix(voname.lower()))
            if name == voname:
                return 5
            elif name.startswith(voname) or voname.startswith(name):
                return 4
            elif name.endswith(voname) or voname.endswith(name):
                return 4
            elif name in voname:
                return 3
            elif voname in name:
                return 2
            else:
                return -1

    @cached_property
    def key(self):
        return self.name_key * 100 + candidate_classes.index(self.__class__)

    @cached_property
    def has_name_similarity(self):
        return self.name_key > 0

    for level in ('info', 'warning', 'error', 'critical'):
        exec(('def {0}(self, msg):\n'+
              '   return logger.{0}(">>> " + msg)\n').format(level))

    def exception(self, e):
        logger.exception(e)

    def __cmp__(self, other):
        if not isinstance(other, Match):
            raise NotImplementedError
        return cmp(self.key, other.key)

    def __repr__(self):
        return ('<%s: %s (from match %s)>'
                % (type(self).__name__, self.url, self.match.group(0)))


class VCSMatch(Match):
    '''Base class for strings like “svn checkout {url}”/“hg clone {url}”/etc'''

    url_norm_subs = ()
    scm_url_norm_subs = ()

    def __init__(self, *args, **kwargs):
        super(VCSMatch, self).__init__(*args, **kwargs)
        self.scm_url = self.match.group(1)
        self.normalize_urls()
        self.name = self.scm_url.rpartition('/')[-1]

    def normalize_urls(self):
        '''Transform some URLs so that they are not different from Github/BitbucketMatch

        Relies on the list of pairs (regex, substitute) located in self.url_norm_subs (for self.url) 
        and self.scm_url_norm_subs (for self.scm_url). Normally it is used to transform something 
        like “https://github.com/author/repo.git” into “git://github.com/author/repo”.
        '''
        for reg, rep in self.scm_url_norm_subs:
            self.scm_url = reg.subn(rep, self.scm_url)[0]
        self.url = self.scm_url
        for reg, rep in self.url_norm_subs:
            self.url = reg.subn(rep, self.url)[0]


class MercurialMatch(VCSMatch):
    '''Matches “hg clone {url}”'''
    re = mercurial_url

    scm = 'hg'

    @cached_property
    def files(self):
        parsing_result = remote_parser.parse_url(self.scm_url, 'tip')
        return set(next(iter(parsing_result['tips'])).files)


class MercurialMatch2(MercurialMatch):
    '''Matches strings like “mercurial [repository is located at] {url}”'''
    re = mercurial_url_2

    scm = 'hg'

    def __init__(self, *args, **kwargs):
        super(MercurialMatch2, self).__init__(*args, **kwargs)
        for attr in ('scm_url', 'name', 'url'):
            setattr(self, attr, getattr(self, attr).rstrip('.,!?'))


class SubversionMatch(VCSMatch):
    '''Matches “svn checkout {url}”'''
    re = subversion_url

    scm = 'svn'

    @cached_property
    def files(self):
        return set(lssvn.list_svn_files(self.scm_url))


class GitMatch(VCSMatch):
    '''Matches “git checkout {url}”'''
    scm_url_norm_subs = (
        (re.compile('^\w+://(?:git@)?github\.com[/:]([^/]+)/([^/]+?)(?:\.git)?(?:/.*)?$'),
         r'git://github.com/\1/\2'),
        (re.compile('^\w+://(?:git@)?bitbucket\.org[/:]([^/]+)/([^/]+?)(?:\.git)?(?:/.*)?$'),
         r'https://bitbucket.org/\1/\2'),
    )

    re = git_url

    scm = 'git'

    @cached_property
    def files(self):
        try:
            return set(lsgit.list_git_files(self.scm_url))
        except Exception as e:
            logger.exception(e)
            return lsgit.list_git_files(self.scm_url, allow_depth=False)


class BitbucketMercurialMatch(MercurialMatch):
    '''Matches strings like “hg clone https://bitbucket.org/{author}/{repo}”'''
    re = bitbucket_mercurial_url

    scm = 'hg'

    def __init__(self, *args, **kwargs):
        super(BitbucketMercurialMatch, self).__init__(*args, **kwargs)
        self.name = self.match.group(2)
        self.scm_url = 'https://bitbucket.org/' + self.match.group(1) + '/' + self.name
        self.url = self.scm_url


class BitbucketMatch(Match):
    '''Matches URLs bitbucket.org/{author}/{repo}'''
    re = bitbucket_noscm_url

    def __init__(self, *args, **kwargs):
        super(BitbucketMatch, self).__init__(*args, **kwargs)
        self.name = self.match.group(2)
        self.repo_path = self.match.group(1) + '/' + self.name
        self.scm_url = 'https://bitbucket.org/' + self.repo_path
        self.url = self.scm_url

    def _check_scm(self):
        self._check_presence()
        try:
            if self.scm_url.endswith('.git'):
                self.info('Assuming {0} is not a mercurial repository'.format(self.scm_url))
                self.scm_url = self.scm_url[:-4]
                raise NotLoggedError
            self.info('Checking whether {0} is a mercurial repository'.format(self.scm_url))
            parsing_result = remote_parser.parse_url(self.scm_url, 'tip')
        except Exception as e:
            self.info('Checking whether {0} is a git repository'.format(self.scm_url))
            self.files = set(lsgit.list_git_files(self.scm_url))
            self.scm = 'git'
        else:
            self.files = set(next(iter(parsing_result['tips'])).files)
            self.scm = 'hg'

    @cached_property
    def files(self):
        self._check_scm()
        return self.files

    @cached_property
    def scm(self):
        self._check_scm()
        return self.scm

    def _check_presence(self, attempt=0):
        c = httplib.HTTPSConnection('bitbucket.org')
        c.request('HEAD', '/' + self.repo_path)
        r = c.getresponse()
        if 400 <= r.status < 500:
            self.error('Cannot use this match: request failed with code %u (%s)'
                       % (r.status, httplib.responses[r.status]))
            raise NotLoggedError
        elif 500 <= r.status:
            if attempt < MAX_ATTEMPTS:
                self.error('Retrying: request failed with code %u (%s)'
                           % (r.status, httplib.responses[r.status]))
                self._check_presence(attempt + 1)
            else:
                self.error('Cannot use this match: request failed with code %u (%s), attempt %u'
                           % (r.status, httplib.responses[r.status], attempt))


class BitbucketSiteMatch(BitbucketMatch):
    '''Matches URLs like {author}.bitbucket.org/{repo}'''
    re = bitbucket_site_url


class CodeGoogleMatch(Match):
    '''Matches code.google.com/(p|r)/{project}'''
    re = codegoogle_url

    def __init__(self, *args, **kwargs):
        super(CodeGoogleMatch, self).__init__(*args, **kwargs)
        self.name = self.match.group(2)
        urlpart = self.match.group(1) + '/' + self.match.group(2)
        url = 'https://code.google.com/' + urlpart
        self.scm_url = url
        self.url = url

    def _check_scm(self):
        try:
            self.info('Checking whether {0} is a mercurial repository'.format(self.scm_url))
            parsing_result = remote_parser.parse_url(self.scm_url, 'tip')
        except Exception as e:
            try:
                self.info('Checking whether {0} is a git repository'.format(self.scm_url))
                self.files = set(lsgit.list_git_files(self.scm_url, allow_depth=False))
                self.scm = 'git'
            except Exception as e:
                self.info('Checking whether {0} is a subversion repository'.format(self.scm_url))
                # FIXME detect directory
                # Plugin for which detection is useful: #2805
                self.scm_url = 'https://' + self.name + '.googlecode.com/svn'
                scm_cache = get_scm_cache()
                try:
                    self.files = scm_cache[self.scm_url]
                    self.debug('Obtained file list from cache for URL {0}'.format(self.scm_url))
                except KeyError:
                    self.files = set(lssvn.list_svn_files(self.scm_url))
                    self.info('Subversion files: {0!r}'.format(self.files))
                    trunkfiles = {tf[6:] for tf in self.files if tf.startswith('trunk/')}
                    if trunkfiles:
                        self.scm_url += '/trunk'
                        self.files = trunkfiles
                        self.info('Found trunk/ directory, leaving only files in there: {0!r}'.format(self.files))
                self.scm = 'svn'
        else:
            self.files = set(next(iter(parsing_result['tips'])).files)
            self.scm = 'hg'


    @cached_property
    def files(self):
        self._check_scm()
        return self.files

    @cached_property
    def scm(self):
        self._check_scm()
        return self.scm


class GithubMatch(Match):
    '''Matches github.com/{author}/{repo} URLs

    Uses github API to get list of files.'''
    re = github_url

    scm = 'git'

    def __init__(self, *args, **kwargs):
        super(GithubMatch, self).__init__(*args, **kwargs)
        self.name = self.match.group(2)
        self.repo_path = self.match.group(1) + '/' + self.name
        if self.repo_path.endswith('.git'):
            self.repo_path = self.repo_path[:-4]
        self.url = 'https://github.com/' + self.repo_path
        self._check_redirects()

    def _check_redirects(self, attempt=0):
        c = httplib.HTTPSConnection('github.com')
        c.request('HEAD', '/' + self.repo_path)
        r = c.getresponse()
        if r.status == httplib.MOVED_PERMANENTLY:
            new_url = r.msg['location']
            assert new_url.startswith('https://github.com/')
            if new_url != self.url:
                self.info('Found redirect to %s' % new_url)
                self.url = new_url
                self.repo_path = self.url[len('https://github.com/'):]
                self.name = self.repo_path.rpartition('/')[-1]
        elif 400 <= r.status < 500:
            self.error('Cannot use this match: request failed with code %u (%s)'
                       % (r.status, httplib.responses[r.status]))
            raise NotLoggedError
        elif 500 <= r.status:
            if attempt < MAX_ATTEMPTS:
                self.error('Retrying: request failed with code %u (%s)'
                           % (r.status, httplib.responses[r.status]))
                self._check_redirects(attempt + 1)
            else:
                self.error('Cannot use this match: request failed with code %u (%s), attempt %u'
                           % (r.status, httplib.responses[r.status], attempt))

        self.scm_url = 'git://github.com/' + self.repo_path

    @cached_property
    def files(self):
        if self.scm_url.startswith('git://github.com/vim-scripts'):
            self.error('removing candidate: vim-scripts repositories are not used by VAM')
            raise NotLoggedError
        return set(lsgh.list_github_files(self.repo_path))


class VundleGithubMatch(GithubMatch):
    '''Matches “Vundle '{author}/{repo}'” strings'''
    re = vundle_github_url


class VundleGithubMatch2(GithubMatch):
    '''Matches strings like “[add] bundle for {author}/{repo}”'''
    re = vundle_github_url_2


class GistMatch(GithubMatch):
    '''Matches gist github URLs

    Uses github API as well, but a different portion of it.'''
    re = gist_url

    scm = 'git'

    def __init__(self, *args, **kwargs):
        Match.__init__(self, *args, **kwargs)
        self.name = self.match.group(1)
        self.scm_url = 'git://gist.github.com/' + self.name
        self.url = 'https://gist.github.com/' + self.name

    @cached_property
    def files(self):
        return set(lsgh.list_gist_files(self.name))


candidate_classes = (
    GithubMatch,
    VundleGithubMatch,
    VundleGithubMatch2,
    GistMatch,
    CodeGoogleMatch,
    BitbucketMercurialMatch,
    GitMatch,
    MercurialMatch,
    MercurialMatch2,
    SubversionMatch,
    BitbucketMatch,
    BitbucketSiteMatch,
)

assert len(candidate_classes) < 100, 'candidate_classes has grown too lengthy, adjust Match.key'


def _find_repo_candidates(voinfo):
    '''Process all Match subclasses recorded above

    Uses MatchSubclass.re to find URLs that are candidates to be repository URLs and yields 
    instances of used MatchSubclass.
    '''
    foundstrings = set()
    for C in candidate_classes:
        for key in ('install_details', 'description'):
            try:
                string = voinfo[key]
            except KeyError:
                logger.error('>> Key {0} was not found in voinfo of script {script_name}'
                             .format(key, **voinfo))
                continue
            for match in C.re.finditer(string):
                if not match.group(0) in foundstrings:
                    foundstrings.add(match.group(0))
                    try:
                        logger.debug('>> Created candidate {0}: {1}'.format(
                            C.__name__, match.group(0)))
                        yield C(match, voinfo)
                    except NotLoggedError:
                        pass


_checked_URLs = {}

def get_scm_file_list(candidate):
    '''Returns candidate.files, cached; cache key is candidate.scm_url'''
    url = candidate.scm_url
    try:
        files = _checked_URLs[url]
        logger.debug('>>> Obtained files from cache for URL {0}'.format(url))
    except KeyError:
        logger.debug('>>> No files in cache for URL {0}'.format(url))
        files = set(candidate.files)
        _checked_URLs[url] = files
    return files


def update_scm_cache(d):
    return _checked_URLs.update(d)


def get_scm_cache():
    return _checked_URLs


def find_repo_candidate(voinfo, vofiles=None):
    '''Find repository URL for the given plugin

    Iterates for all candidates returned by _find_repo_candidates and returns first candidate with 
    best score found.'''
    global _checked_URLs
    candidates = sorted(_find_repo_candidates(voinfo), key=lambda o: o.key, reverse=True)
    best_candidate = None
    already_checked = set()
    for candidate in candidates:
        if vofiles is None:
            vofiles = get_file_list(voinfo)
            vofiles = {fname for fname in vofiles if not fname.endswith('/')}
            logger.info('>> vim.org files: ' + repr(vofiles))
        url = candidate.url
        if url in already_checked:
            logger.debug('>>> Omitting {0} because it was already checked'.format(url))
            continue
        already_checked.add(url)
        logger.info('>> Checking candidate {0} with key {1}: {2}'.format(
            candidate.__class__.__name__,
            candidate.key,
            candidate.match.group(0)
        ))
        try:
            files = get_scm_file_list(candidate)
            logger.info('>>> Repository files: {0!r}'.format(files))
            prefix, score = compare_file_lists(candidate, vofiles, files)
        except NotLoggedError:
            pass
        except Exception as e:
            logger.exception(e)
        else:
            candidate.prefix = prefix
            if score == 100:
                logger.info('>> Found candidate {0}: {1} (100)'.format(candidate.__class__.__name__,
                                                                    candidate.match.group(0)))
                return candidate
            elif score and (not best_candidate or score > best_candidate.score):
                best_candidate = candidate
                best_candidate.score = score
    if best_candidate:
        logger.info('Found candidate {0}: {1} ({2})'.format(
                                                best_candidate.__class__.__name__,
                                                best_candidate.match.group(0),
                                                best_candidate.score))
    return best_candidate


def set_remote_parser(rp):
    '''Set global remote_parser object to a given value

    This object is assumed to have class MercurialRemoteParser. It is responsibility of caller to 
    run its __enter__ and __exit__ methods.
    '''
    global remote_parser
    remote_parser = rp


scm_matches = {
    'svn': SubversionMatch,
    'git': GitMatch,
    'hg':  MercurialMatch,
}


# vim: tw=100 ft=python fenc=utf-8 ts=4 sts=4 sw=4
