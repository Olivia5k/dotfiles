#!/usr/bin/env python

import sys

from urllib.parse import quote
import subprocess as sub

BROWSER = '/usr/bin/uzbl-browser'

ENGINES = {
    'g': 'google',
    't': 'tvtropes',
    'w': 'wikipedia',
    'y': 'youtube',
    'l': 'localhost'
}

def browser(url):
    print(BROWSER, url)
    b = sub.Popen([BROWSER, url], stdout=sub.PIPE, stderr=sub.PIPE)

def urlize(args):
    return quote(' '.join(args))

def google(args):
    return 'http://www.google.com/search?q=' + urlize(args)

def wikipedia(args):
    pass

def tvtropes(args=None):
    pass

def localhost(args):
    return 'http://localhost:8000'

def main():
    url = None

    if len(sys.argv) > 1 and len(sys.argv[1]) == 1:
        url = globals()[ENGINES[sys.argv[1]]](sys.argv[1:])
    elif len(sys.argv) > 2 or len(sys.argv) == 2 and '.' not in sys.argv[1]:
        url = google(sys.argv[1:])
    else:
        url = 'http://' + sys.argv[1]

    if url:
        browser(url)
        return 0
    else:
        print('Error: URL missing or malformed.')
        return 1

if __name__ == '__main__':
    sys.exit(main())
