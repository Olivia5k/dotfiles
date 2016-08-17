# vim:fileencoding=utf-8

from __future__ import division

import os
import logging
import codecs
import json
import re
import urllib

import bz2
import gzip
import tarfile
import zipfile
import io

import magic

try:
    import lzma
except ImportError:
    from backports import lzma


logger = logging.getLogger('vimorg')


def getdb():
    if os.path.isfile('script-info.json'):
        logger.info('Using file script-info.json')
        with codecs.open('script-info.json', 'r', encoding='utf-8') as F:
            return json.load(F)
    else:
        logger.info('Processing http://www.vim.org/script-info.php')
        return json.load(urllib.urlopen('http://www.vim.org/script-info.php'))


def get_ext(fname):
    return fname.rpartition('.')[-1]


def get_patchinfo_fix_dirs(ret={}):
    if ret:
        return ret
    else:
        with open(os.path.join('.', 'db', 'patchinfo.vim')) as PIF:
            start_section = b'"▶1 '
            in_section = 0
            line_re = re.compile(r"(\d+).*: '([^']+)'")
            for line in PIF:
                if line.startswith(start_section):
                    if line.startswith(start_section + b'Type corrections'):
                        in_section = 2
                    elif line.startswith(start_section + b'Fixing target directories'):
                        in_section = 1
                    else:
                        if in_section:
                            break
                        in_section = 0
                    continue
                if in_section:
                    match = line_re.search(line)
                    if match:
                        d = match.group(2)
                        ret[match.group(1)] = d if in_section == 1 else _guess_fix_dir(d)
        return ret


def _guess_fix_dir(script_type):
    if script_type in ('syntax', 'indent', 'ftplugin'):
        return script_type
    elif script_type == 'color scheme':
        return 'colors'
    else:
        return 'plugin'


def guess_fix_dir(voinfo):
    patchinfo_fix_dirs = get_patchinfo_fix_dirs()
    try:
        return patchinfo_fix_dirs[voinfo['script_id']]
    except KeyError:
        return _guess_fix_dir(voinfo['script_type'])


def get_tar_names(tf):
    for ti in tf:
        if not ti.isdir():
            yield ti.name


# Namespace
class FileListers:
    @staticmethod
    def gz(AF):
        # GzipFile requires tell
        return io.BytesIO(gzip.GzipFile(fileobj=AF).read())

    @staticmethod
    def bz2(AF):
        return io.BytesIO(bz2.decompress(AF.read()))

    @staticmethod
    def lzma(AF):
        return io.BytesIO(lzma.LZMAFile(AF).read())

    @staticmethod
    def xz(AF):
        return io.BytesIO(lzma.LZMAFile(AF).read())

    @staticmethod
    def tar(AF):
        # tar requires tell on its own
        return set(get_tar_names(tarfile.TarFile(fileobj=AF)))

    @staticmethod
    def tgz(AF):
        # gz requires tell
        return set(get_tar_names(tarfile.TarFile(fileobj=FileListers.gz(AF), format='gz')))

    @staticmethod
    def tbz(AF):
        return set(get_tar_names(tarfile.TarFile(fileobj=FileListers.bz2(AF), format='bz2')))
    tbz2 = tbz

    @staticmethod
    def txz(AF):
        return set(get_tar_names(tarfile.TarFile(fileobj=FileListers.xz(AF))))

    @staticmethod
    def zip(AF):
        # ZipFile requires seek
        zf = zipfile.ZipFile(AF)
        ret = set(zf.namelist())
        if len(ret) == 1:
            fname = next(iter(ret))
            if '/' not in fname and hasattr(FileListers, get_ext(fname)):
                logger.debug('>>>> Assuming somebody packed an archive ({0}) into a zip file'.format(fname))
                return io.BytesIO(zf.open(fname).read())
        return ret

    @staticmethod
    def vmb(AF):
        af = iter(AF)
        while not next(af).startswith('finish'):
            pass

        files = set()
        filere = re.compile('^\S+')
        try:
            while True:
                files.add(filere.match(next(af)).group(0))
                numlines = int(next(af))
                while numlines:
                    next(af)
                    numlines -= 1
        except StopIteration:
            pass
        return files

    vba = vmb


def get_voinfo_hash(voinfo):
    return hash(voinfo.get('description',     '') + voinfo.get('install_details', ''))


mime_to_ext = {
    'application/x-gzip': 'gz',
    'application/x-tar': 'tar',
}

_magic = None

def find_mime(AF):
    global _magic
    if not _magic:
        _magic = magic.open(magic.MAGIC_MIME_TYPE)
        _magic.load('/usr/share/misc/magic.mgc')
    af = AF.read()
    mime = _magic.buffer(af)
    logger.info('>>>> Deduced mime extension: {0}'.format(mime))
    return io.BytesIO(af), mime_to_ext[mime]


def _get_file_list(AF, ext, aname, had_to_guess=False):
    try:
        ret = getattr(FileListers, ext)(AF)
    except Exception as e:
        # It may appear that archive has wrong extension (e.g. like in script #4734 that reports 
        # being .tar file while it is actually .tar.gz). Thus here is catch-all rule.
        # TODO: Maybe add patchinfo_generated.json which will contain archive name corrections for 
        #       such cases.
        logger.exception(e)
        if not had_to_guess:
            AF.seek(0)
            AF, ext = find_mime(AF)
            ret = _get_file_list(AF, ext, aname, True)
            had_to_guess = True
        else:
            raise
    if not isinstance(ret, set):
        if had_to_guess:
            AF, ext = find_mime(ret)
            return _get_file_list(AF, ext, None, had_to_guess)
        else:
            aname = aname[:-1-len(ext)]
            ext = get_ext(aname).lower()
            return _get_file_list(ret, ext, aname, had_to_guess)
    else:
        return ret


_downloaded_URLs = {}

def get_file_list(voinfo):
    rinfo = voinfo['releases'][0]
    aurl = 'http://www.vim.org/scripts/download_script.php?src_id='+rinfo['src_id']
    try:
        logger.debug('>>>> Got %s from cache' % aurl)
        return _downloaded_URLs[aurl]
    except KeyError:
        pass
    aname = rinfo['package']
    ext = get_ext(aname).lower()
    logger.info('>>> Processing archive %s (ext %s)' % (aname, ext))
    if ext == 'vim':
        ret = {guess_fix_dir(voinfo) + '/' + aname}
    elif ext in FileListers.__dict__:
        ret = {fname for fname in _get_file_list(io.BytesIO(urllib.urlopen(aurl).read()), ext, aname)
                         if not (fname.startswith('__MACOSX') or '.git/' in fname
                                 or fname.startswith('._') or '/._' in fname
                                 or fname.endswith('.DS_Store'))}
    else:
        raise ValueError('Unknown extension')
    _downloaded_URLs[aurl] = ret
    return ret


def update_vo_cache(d):
    return _downloaded_URLs.update(d)


def get_vo_cache():
    return _downloaded_URLs


# Only directories that may be automatically loaded by vim are listed below.
specialdirs = {'plugin', 'ftplugin', 'syntax', 'indent', 'after'}

def isvimvofile(fname):
    return ((fname.endswith('.vim') and fname.partition('/')[0] in specialdirs))
           #or (fname.endswith('.py') and
           #    (fname.startswith('pythonx/')
           #        or fname.startswith('python2/')
           #        or fname.startswith('python3/'))))


expected_extensions = {'vim', 'txt', 'py', 'pl', 'lua', 'pm', 'rb'}
def isexpected(fname):
    # Second condition blocks files like .hg_archival.txt
    return ((get_ext(fname) in expected_extensions)
            and not fname.startswith('.hg')
            and not fname.startswith('.git')
            and not 'README' in fname)


def check_vim_files(vimvofiles, vimfiles):
    if vimvofiles < vimfiles and len(vimfiles) - len(vimvofiles) > 5:
        logger.info('>>>> Rejected because there are more then 5 significant files '
                'not present in archive: %s'
                % (repr(vimfiles - vimvofiles)))
        return True
    return False


# Directories corresponding to plugin types on www.vim.org
vodirs = {'plugin', 'colors', 'ftplugin', 'indent', 'syntax'}
def compare_file_lists(candidate, vofiles, files, prefix=None):
    expvofiles = {fname for fname in vofiles if isexpected(fname)}
    expfiles = {fname for fname in files if isexpected(fname)}
    vimvofiles = {fname for fname in expvofiles if isvimvofile(fname)}
    vimfiles = {fname for fname in files if isvimvofile(fname)}
    if vofiles and files and vofiles <= files:
        if check_vim_files(vimvofiles, vimfiles):
            return (prefix, 0)
        logger.info('>>>> Accepted with score 100 because all files '
                'are contained in the repository')
        return (prefix, 100)
    elif expvofiles and expfiles and expvofiles <= expfiles:
        if check_vim_files(vimvofiles, vimfiles):
            return (prefix, 0)
        logger.info('>>>> Accepted with score 90 because all files that are considered significant '
                'are contained in the repository: %s. Missing insignificant files: %s.'
                % (repr(expvofiles), repr(vofiles - files)))
        return (prefix, 90)
    elif (candidate.has_name_similarity and
            (len(vofiles ^ files) / ((len(vofiles) + len(files)) / 2)) <= 0.2):
        if check_vim_files(vimvofiles, vimfiles):
            return (prefix, 0)
        logger.info('>>>> Accepted with score 80 because difference between repository and archive '
                'is not greater then 20%% of files. Files not present in the repository: %s. '
                'Files not present in the archive: %s.'
                % (repr(vofiles - files), repr(files - vofiles)))
        return (prefix, 80)
    elif (candidate.has_name_similarity and
            (len(expvofiles ^ expfiles) / ((len(expvofiles) + len(expfiles)) / 2)) <= 0.1):
        if check_vim_files(vimvofiles, vimfiles):
            return (prefix, 0)
        logger.info('>>>> Accepted with score 70 because difference between repository and archive '
                'is not greater then 10%% of files that are considered significant. '
                'Files not present in the repository: %s. '
                'Files not present in the archive: %s. '
                'Significant files not present in the repository: %s. '
                'Significant files not present in the archive: %s.'
                % (repr(vofiles - files), repr(files - vofiles),
                   repr(expvofiles - expfiles), repr(expfiles - expvofiles)))
        return (prefix, 70)
    else:
        vofileparts = [fname.partition('/') for fname in vofiles]
        leadingdir = vofileparts[0][0]
        if (leadingdir
                and leadingdir not in vodirs
                and all((part[0] == leadingdir for part in vofileparts[1:]))
                and all((part[1] for part in vofileparts))):
            logger.info('>>>> Trying to match with leading path component removed: %s'
                    % leadingdir)
            return compare_file_lists(
                candidate,
                {part[-1] for part in vofileparts},
                files,
                leadingdir,
            )
        else:
            # TODO Detect the need in vamkr#AddCopyHook
            # TODO Detect the need in fixing target directory (example: script #4769 which has 
            #      a single .vim file “archive” that has to be put under autoload/airline/themes as 
            #      listed in db/patchinfo.vim). Add this in patchinfo_generated.json.
            logger.info('>>>> Rejected because not all significant files were found '
                    'in the repository: %s' % repr(expvofiles - files))
            return (prefix, 0)
