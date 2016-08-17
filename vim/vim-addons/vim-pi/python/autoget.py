#!/usr/bin/env python
# vim:fileencoding=utf-8
'''
Utility for processing non-standardased data found on www.vim.org.

Usage:
    autoget.py [-d] [-c | -w] last [-nif] <N>
    autoget.py [-d] [-c | -w] new [-nDi]
    autoget.py [-d] [-c | -w] recheck
    autoget.py [-d] [-c | -w] scripts [-nif] <SID>...
    autoget.py [-d] [-c | -w] annotate [--no-extra-check --print-other-candidate]
    autoget.py -h | --help

Options:
    -h --help         Show this screen
    -d --debug        Print additional output

    -c --use-cache    Use cache. Cache is located in ./cache.pickle.
    -w --write-cache  Merge cache from ./cache.pickle with data obtained
                      in the current run. Do not use cache from
                      ./cache.pickle though.
Commands:
    last              Process last N script numbers.
    scripts           Process given scripts.
        -f --force    Normally autoget.py omits reprocessing scripts which were
                      already processed. This option makes autoget.py process
                      them regardless.
                      Already processed scripts are scripts mentioned in
                        ∙ ./db/scm_generated.json
                        ∙ ./db/not_found.json
                        ∙ ./db/scmsources.vim
                        ∙ ./db/omitted.json

    new               Check scripts with script numbers above the greatest number
                      found in aforementioned list (list attached to -f argument).
        -D --no-descriptions
                      Do not check description hashes.

    The following options are supported by “last”, “scripts” and “new” subcommands:
        -n --dry-run  Do not write data to ./db/*.
        -i --interrupt-write
                      Write to ./db/* on interrupt.

    recheck           Recheck contents of ./db/scm_generate.json.

    annotate          Annotate scripts found in ./db/scmsources.vim.
        -x --no-extra-check
                      Do not check lines that contain information that autoget.py
                      is not able to generate.
        -p --print-other-candidate
                      If autoget.py found candidate other then present in
                      ./db/scmsources.vim print it.

When searching for candidates autoget.py checks plugin description and
installation details for common repository URL patterns. When such pattern
is found it compares list of files present in given repository URL with
the list of files present in www.vim.org archive. If lists match (exact match
is not required) then URL is considered to point to VCS-managed version of
the plugin.

Files used by this utility:
    ./db/scm_generated.json   Contains found candidates. This file is generated
                              automatically.
    ./db/not_found.json       Contains a list of script numbers which were
                              processed, but with no candidates found. This file
                              is generated automatically.
    ./db/omitted.json         Contains a list of plugins which should not be
                              processed by autoget.py. This file is populated
                              manually.
    ./db/scmsources.json      Contains SCM sources found by VAM-kr contributors.
                              This file is populated manually.
    ./db/description_hashes.json
                              Contains hashes of description + installation
                              details. Used to determine whether plugin
                              description changed: when it changes autoget.py
                              rechecks plugin.
    ./cache.pickle            Contains cached lists of files.
    ./script-info.json        Contains data downloaded from
                              http://www.vim.org/script-info.php.
                              If it is present then script uses it in place of
                              downloading data from the above URL.

Currently supports git, mercurial and subversion repositories.
'''
from __future__ import unicode_literals, division, print_function

import yaml
import os
import json
import re
import logging
import sys
import argparse
from functools import partial
import pickle
import docopt

sys.path.append(os.path.dirname(__file__))

import list_hg_files as lshg
import list_github_files as lsgh

from vimorg import (getdb, get_file_list, compare_file_lists, get_voinfo_hash,
                    update_vo_cache, get_vo_cache)
from vomatch import (NotLoggedError, update_scm_cache, get_scm_cache, find_repo_candidate,
                     set_remote_parser, scm_matches, get_scm_file_list)

logger = logging.getLogger('autoget')


cache_name = os.path.join('.', 'cache.pickle')
scmsources_name = os.path.join('.', 'db', 'scmsources.vim')
scm_generated_name = os.path.join('.', 'db', 'scm_generated.json')
not_found_name = os.path.join('.', 'db', 'not_found.json')
omitted_name = os.path.join('.', 'db', 'omitted.json')
description_hashes_name = os.path.join('.', 'db', 'description_hashes.json')


def dump_json(obj, F):
    '''Dump object to JSON in a VCS- and human-friendly way

    VCS-friendly implies having newlines and dictionary key sorting, without spaces after commas.
    Human-friendly implies using indentation and spaces after colons.
    '''
    return json.dump(obj, F, indent=2, sort_keys=True, separators=(',', ': '))


def dump_json_nr_set(st, F):
    '''Dump set() to json as a sorted list in a VCS- and human-friendly way

    set() is supposed to contain only string integer keys (i.e. integers represented as strings). 
    For the definition of VCS- and human-friendly check out dump_json above.
    '''
    dump_json(list(sorted(st, key=int)), F)


def load_scmnrs_json(scmnrs, fname, typ=dict):
    '''Load json file, recording its contents into scmnrs

    If file is not present instance of typ is returned. If file is present loaded object is 
    converted to the given typ.
    '''
    try:
        with open(fname) as F:
            ret = json.load(F)
            scmnrs.update(ret)
            return typ(ret)
    except IOError:
        return typ()


def candidate_to_sg(candidate):
    '''Convert candidate to dictionary suitable for recording into scm_generated.json'''
    return {'type': candidate.scm, 'url': candidate.scm_url}


def process_voinfo(scm_generated, found, not_found, description_hashes, key, voinfo, recheck=False):
    '''Process given plugin

    If ``recheck`` is ``True`` then it only prints to stderr whether something changed.

    If ``recheck`` is ``False`` then it records found URL in scm_generated and puts corresponding 
    plugin number into ``found``. If no URL was found it records plugin number into ``not_found``.
    '''
    logger.info('> Checking plugin {script_name} (vimscript #{script_id})'
                .format(**voinfo))
    try:
        candidate = find_repo_candidate(voinfo)
        if recheck:
            desc = '%s : %s' % (voinfo['script_id'], voinfo['script_name'])
            if candidate:
                c_sg = candidate_to_sg(candidate)
                s_sg = scm_generated[voinfo['script_id']]
                if c_sg == s_sg:
                    print ('== ' + desc)
                else:
                    logger.info('> {0!r} (new) /= {1!r} (old)'.format(c_sg, s_sg))
                    print ('/= ' + desc)
            else:
                print ('no ' + desc)
        else:
            if candidate:
                scm_generated[key] = candidate_to_sg(candidate)
                logger.info('> Recording found candidate for {0}: {1}'
                            .format(key, scm_generated[key]))
                not_found.discard(key)
            else:
                logger.info('> Recording failure to find candidates for {0}'.format(key))
                not_found.add(key)
            found.add(key)
            if not args['--no-descriptions']:
                description_hashes[key] = get_voinfo_hash(voinfo)
    except Exception as e:
        logger.exception(e)


def process_scmsources(return_scmnrs=True):
    scmnrs = set()
    scmnr_lines = []
    with open(scmsources_name) as SF:
        scmnr_re = re.compile(r'^let scmnr\.(\d+)')
        for line in SF:
            match = scmnr_re.match(line)
            if match:
                scmnrs.add(match.group(1))
                scmnr_lines.append(line)
    return scmnrs if return_scmnrs else scmnr_lines


def main():
    scmnrs = process_scmsources()

    omitted       = load_scmnrs_json(scmnrs, omitted_name)
    found = scmnrs.copy()
    scm_generated = load_scmnrs_json(scmnrs, scm_generated_name)
    not_found     = load_scmnrs_json(scmnrs, not_found_name, set)

    if not args['--no-descriptions']:
        try:
            with open(description_hashes_name) as DHF:
                description_hashes = json.load(DHF)
        except IOError:
            description_hashes = {}
    else:
        description_hashes = {}

    if args['scripts']:
        keys = args['<SID>']
        not_found -= set(keys)
    elif args['last']:
        i = args['last']
        _keys = reversed(sorted(db, key=int))
        keys = []
        while i:
            keys.append(next(_keys))
            i -= 1
    else:
        keys = reversed(sorted(db, key=int))

    _process_voinfo = partial(process_voinfo, scm_generated,found,not_found,description_hashes)
    with lshg.MercurialRemoteParser() as remote_parser:
        set_remote_parser(remote_parser)
        if args['recheck']:
            for key in scm_generated:
                _process_voinfo(key, db[key], recheck=True)
        else:
            for key in keys:
                if not args['--force'] and key in scmnrs:
                    if args['new']:
                        break
                    else:
                        continue
                logger.info('Considering key {0}'.format(key))
                _process_voinfo(key, db[key])

            if not args['--no-descriptions']:
                logger.info('Starting descriptions check')
                for key in keys:
                    voinfo = db[key]
                    changed = False
                    if key not in description_hashes:
                        h = get_voinfo_hash(voinfo)
                        description_hashes[key] = h
                        changed = True
                    if key in found:
                        continue
                    if not changed:
                        h = get_voinfo_hash(voinfo)
                        changed = (h != description_hashes.get(key))
                        if changed:
                            logger.info('Hash for key {0} changed, checking it'.format(key))
                            description_hashes[key] = h
                    else:
                        logger.info('New hash for key {0}'.format(key))
                    if changed:
                        _process_voinfo(key, voinfo)

    return scm_generated, not_found, description_hashes


def write(msg):
    '''Write msg to sys.stdout, encoding it to utf-8

    If msg contains newline this function also calls .flush().'''
    sys.stdout.write(msg.encode('utf-8'))
    if '\n' in msg:
        sys.stdout.flush()


def prnt(msg):
    '''Like write(), but adds newline to the end of the message'''
    return write(msg + '\n')


def annotate_scmsources():
    url_regex = re.compile('^(.*)$')
    line_regex_strict = re.compile(r"^let\s+scmnr\.(\d+)\s*=\s*\{'type':\s*'(\w+)',\s*'url':\s*'([^']+)'\s*\}\s*")
    line_snr_regex = re.compile(r'(\d+)')
    line_url_regex = re.compile(r"'url': '([^']+)'")
    line_scm_regex = re.compile(r"'type': '([^']+)'")
    prnt ('┌ X if line contains information besides repository URL and SCM used')
    prnt ('│┌ ? or ! if line failes to be parsed by regular expressions')
    prnt ('││┌ A if scm type is unknown')
    prnt ('│││┌ F if match failes')
    prnt ('││││┌ O if match deduced from the description is different, M if there are no')
    prnt ('│││││┌ E if exception occurred, C if printing candidate on this line')
    prnt ('┴┴┴┴┴┴┬─────────────────────────────────────────────────────────────────────────')
    with lshg.MercurialRemoteParser() as remote_parser:
        set_remote_parser(remote_parser)
        for line in process_scmsources(False):
            try:
                numcolumns = 5
                best_candidate = None
                line = line.decode('utf-8')
                logger.info('Checking line ' + line)
                match = line_regex_strict.match(line)
                if match:
                    write('  ')
                    numcolumns -= 2
                    snr, scm, url = match.groups()
                else:
                    write('X')
                    numcolumns -= 1
                    if args['--no-extra-check']:
                        raise NotLoggedError
                    snr = line_snr_regex.search(line)
                    scm = line_scm_regex.search(line)
                    url = line_url_regex.search(line)
                    if snr:
                        snr = snr.group(1)
                    if scm:
                        scm = scm.group(1)
                    if url:
                        url = url.group(1)
                    if not snr:
                        write('!')
                        numcolumns -= 1
                        raise ValueError('snr key not found')
                    if not scm:
                        write('?A')
                        numcolumns -= 2
                        url = None
                    elif not url:
                        write('?-')
                        numcolumns -= 2
                        scm = None
                    else:
                        write(' ')
                        numcolumns -= 1
                if scm not in scm_matches:
                    write('A')
                    numcolumns -= 1
                    url = None
                    scm = None
                else:
                    write(' ')
                    numcolumns -= 1

                voinfo = db[snr]
                vofiles = set(get_file_list(voinfo))
                if url:
                    match = url_regex.match(url)
                    candidate = scm_matches[scm](match, voinfo)
                    files = get_scm_file_list(candidate)
                    prefix, key2 = compare_file_lists(candidate, vofiles, files)
                    write(' ' if key2 else 'F')
                    numcolumns -= 1
                else:
                    write('-')
                    numcolumns -= 1

                best_candidate = find_repo_candidate(voinfo, vofiles)
                if not best_candidate:
                    write('M')
                    numcolumns -= 1
                elif not (url and best_candidate.scm_url == url and best_candidate.scm == scm):
                    write('O')
                    numcolumns -= 1
                else:
                    write(' ')
                    numcolumns -= 1
                    best_candidate = None
                write(' ')
                numcolumns -= 1
            except Exception as e:
                if not isinstance(e, NotLoggedError):
                    logger.exception(e)
                write(('-' * numcolumns) + 'E')
            finally:
                # XXX line ends with \n, thus not writing `+ '\n'` here.
                write('│' + line)
                if best_candidate and args['--print-other-candidate']:
                    write("-----C│let scmnr.%-4u = {'type': '%s', 'url': '%s'}\n"
                            % (int(snr), best_candidate.scm, best_candidate.scm_url))


if __name__ == '__main__':
    args = docopt.docopt(__doc__)

    if (args['scripts'] or args['recheck'] or args['last']):
        args['--no-descriptions'] = True

    if args['--use-cache'] or args['--write-cache']:
        try:
            with open(cache_name) as CF:
                scm_cache, vo_cache = pickle.load(CF)
                if args['--use-cache']:
                    update_scm_cache(scm_cache)
                    update_vo_cache(vo_cache)
        except IOError:
            pass

    root_logger = logging.getLogger()
    root_logger.setLevel(logging.DEBUG if args['--debug'] else logging.INFO)
    handler = logging.StreamHandler()
    root_logger.addHandler(handler)

    db = getdb()

    with open(os.path.expanduser('~/.settings/passwords.yaml')) as PF:
        passwords = yaml.load(PF)
        user, password = iter(passwords['github.com'][0].items()).next()
    lsgh.init_gh(user, password)

    ret = 0
    write_cache = False
    write_db = False
    try:
        if args['annotate']:
            if args['--interrupt-write']:
                write_cache = True
            annotate_scmsources()
            write_cache = True
        else:
            if args['--interrupt-write']:
                write_cache = True
                write_db = True
            scm_generated, not_found, description_hashes = main()
            write_cache = True
            write_db = True
    except Exception as e:
        logger.exception(e)
        ret = 1
    except KeyboardInterrupt:
        pass

    if write_cache and (args['--use-cache'] or args['--write-cache']):
        if args['--use-cache']:
            scm_cache = get_scm_cache()
            vo_cache = get_vo_cache()
        else:
            scm_cache.update(get_scm_cache())
            vo_cache.update(get_vo_cache())
        with open(cache_name, 'w') as CF:
            pickle.dump((scm_cache, vo_cache), CF)

    if write_db and not args['--dry-run'] and 'scm_generated' in locals():
        # FIXME: Should actually handle KeyboardInterrupt in main()
        with open(scm_generated_name, 'w') as SGF:
            dump_json(scm_generated, SGF)

        with open(not_found_name, 'w') as NF:
            dump_json_nr_set(list(not_found), NF)

        if not args['--no-descriptions']:
            with open(description_hashes_name, 'w') as DHF:
                dump_json(description_hashes, DHF)

    sys.exit(ret)


# vim: tw=100 ft=python fenc=utf-8 ts=4 sts=4 sw=4
