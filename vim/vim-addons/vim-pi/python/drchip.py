#!/usr/bin/env python
# vim:fileencoding=utf-8

import urllib
import re
import json
import os

dcregex = re.compile(r'[^a-zA-Z_]')

def getdrchip():
    return urllib.urlopen('http://www.drchip.org/astronaut/vim/forgit.tuv')

def drchip_to_source(s):
    name, url, version = s.strip().split('|')
    name = dcregex.subn('', name)[0] + '@drchip'
    return name, {
        'type': 'archive',
        'url': url,
        'version': version,
        'homepage': 'http://www.drchip.org/astronaut/vim/index.html',
    }

def create_db():
    result = {}
    for line in getdrchip():
        name, source = drchip_to_source(line)
        result[name] = source
    return result

def write_db(db):
    with open(os.path.join('db', 'drchip.json'), 'w') as F:
        json.dump(db, F, indent=2, sort_keys=True, separators=(',', ': '))

if __name__ == '__main__':
    write_db(create_db())
