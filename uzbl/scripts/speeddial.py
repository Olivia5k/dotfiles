#!/usr/bin/env python
# -*- coding: utf-8 -*-
import feedparser

from jinja2 import Template
import sys
import time

sitedict = {
    'iGoogle':"google.com/ig",
    'Uzbl':"www.uzbl.org",
    'Xkcd':"xkcd.com",
    'Explosm':"explosm.net",
    'Archlinux':"archlinux.org",
    'Gmail':"www.gmail.com",
    'Blog':"blog.valkertown.org/wp-admin",
    'Brainbird':"brainbird.co.cc",
    'Lodge It':"http://paste.pocoo.org/",
}

#MY_DELICIOUS_PUBLIC_RSS = "http://..."
#MY_DELICIOUS_INBOX_RSS = "http://..."
sitelist = []
#entries = feedparser.parse(MY_DELICIOUS_PUBLIC_RSS)
#inbox = feedparser.parse(MY_DELICIOUS_INBOX_RSS)
#keys = sitedict.keys()
#keys.sort()
#for key in keys:
    #sitelist.append({'title':key,'url':sitedict[key]})

colors = {
    'boxesbg':'#B5C489',
    'hbg':'#607559',
    'afg':'#7B913D',
    'abg':'#213814',
}

now = time.ctime()
template = Template(
    u"""<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN">
<html>
<head>
<title>Uzbl Speed Dial</title>
<style type="text/css">
body{
 background-color: {{ colors.hbg }};
 background-repeat: no-repeat;
 padding:0;
}
.boxes, .links {
 margin: 60px auto;
 width: 425px;
 padding: 25px;
 overflow-vertical: auto;
 -webkit-border-radius: 15px;
 background-color: {{ colors.boxesbg }};
}
h1,h4 {
 margin-top:15px;
 text-align: center;
 line-height:20px;
 font-size:20px;
 font-variant: small-caps;
}
h1 {
  margin-top:15px;
  font-weight:bold;
  font-size:25px;
  color: #fff;
  margin-bottom: 15px;
}
.boxes {
 height: 475px;
 line-height:60px;
 clear:both;
}

a, a:visited, a:hover {
 text-decoration:none;
 font-weight: normal;
 color: {{ colors.afg }};
}
a:hover {
 font-weight: bold;
}
.box {
 text-align:center;
 background-color: {{ colors.abg }};
 margin: 5px;
 width: 200px;
 float:left;
 height: 60px;
 background-color: {{ colors.boxbg }};
 -webkit-border-top-left-radius: 10px;
 -webkit-border-bottom-right-radius: 10px;

}
</style>
</head>
<body>
<h1>Uzbl Deepspawn</h1>
<div class="boxes">
<h4>Link Boxes</h4>
{% for site in sitelist %}
<div class="box">
<a href="http://{{ site.url }}"> {{ site.title }} </a>
</div>
{% endfor %}
</div>
<div class="links">
<h4>Delicious Bookmarks</h4>
<ul>
{% for entry in bookmarks %}
<li><a href="{{entry.link}}">{{entry.title}}</a></li>
{% endfor %}
</ul>
<h4> Delicious Inbox </h4>
<ul>
{% for entry in inbox %}
<li><a href="{{entry.link}}">{{entry.title}}</a></li>
{% endfor %}
</ul>
</div>
{{ now }}
</body>
</html> """)

print (template.render(
        sitelist=sitelist,
        colors = colors,
        bookmarks = None,
        inbox = None,
        now = now)).encode('utf-8')
