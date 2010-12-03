#!/usr/bin/env python

# USAGE
# load_finish_handler should be set to something like:
# spawn $XDG_DATA_HOME/uzbl/scripts/favicon.py
#
# And remember to: mkdir -p $XDG_CACHE_HOME/uzbl/favicon

# BUGS
# The only case not handled satisfactorily is where the icon is unset in the
# config, and the site has no favicon. This means sites with no favicon will
# show the previous site's favicon.

import os, re, socket, sys, urllib2, urlparse

def urid(cache, uri):
    '''cache='/root', uri='http://holizz.com/favicon.ico'
    => /root/http/holizz.com/favicon.ico'''
    u=urlparse.urlsplit(uri)
    p=u.path.split('/')[1:]
    return os.path.join(cache, u.scheme, u.netloc, *p)

def socksend(sock, input):
    s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    s.connect(sock)
    s.send(input)
    output = ''
    while '\n' not in output:
        output += s.recv(1024)
    s.close()
    return output

def defaulticon(config):
    icon = None
    with open(config, 'r') as c:
        for line in c.read().split('\n'):
            m = re.match(r'^\s*set\s+icon\s*=\s*(.*?)\s*$', line)
            if m:
                icon = m.group(1)
    return icon

if __name__ == '__main__':
    config = sys.argv[1]
    fifo = sys.argv[4]
    sock = sys.argv[5]
    uri = sys.argv[6]

    if 'XDG_CACHE_HOME' in os.environ.keys() and os.environ['XDG_CACHE_HOME']:
        cache_d = os.path.join(os.environ['XDG_CACHE_HOME'],'uzbl/favicon/')
    else:
        cache_d = os.path.join(os.environ['HOME'],'.cache/uzbl/favicon/')

    if not os.path.isdir(cache_d):
        os.makedirs(cache_d)

    # Check for <link rel=icon>
    data = socksend(sock, '''js var m=document.getElementsByTagName('link');for (var n=0;n<m.length;n++){var a=m[n].attributes;if(a.getNamedItem('rel').nodeValue.match(/(^|\s)icon($|\s)/i)){a.getNamedItem('href').nodeValue;break;}}\n''').strip()

    favicon_u = None
    if data:
        favicon_u = urlparse.urljoin(uri, data)

    # Check for /favicon.ico
    if not favicon_u:
        favicon_u = urlparse.urljoin(uri, '/favicon.ico')

    if favicon_u == 'file:///favicon.ico' or favicon_u == '/favicon.ico':
        favicon_f = None
    else:
        favicon_f = urid(cache_d, favicon_u)
        # Download file if it doesn't exist
        if not os.path.isfile(favicon_f):
            favicon_d = os.path.join(*os.path.split(favicon_f)[:-1])
            try:
                input = urllib2.urlopen(favicon_u)
                if not os.path.isdir(favicon_d):
                    os.makedirs(favicon_d)
                with open(favicon_f, 'w') as output:
                    output.write(input.read())
            except urllib2.HTTPError:
                pass

        if not os.path.isfile(favicon_f):
            favicon_f = None

    # Reset icon if site has no favicon
    from_config = False
    if not favicon_f:
        favicon_f = defaulticon(config)
        from_config = True

    # Render the icon
    if (from_config and favicon_f!=None) or (favicon_f and os.path.isfile(favicon_f)):
        open(fifo,'a').write('set icon = %s\n' % favicon_f)
