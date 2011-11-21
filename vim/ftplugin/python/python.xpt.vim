XPTemplate priority=personal

XPT d " debug
debug(`^)
XPT di " dict construct
`name^ = {
    '`key^': `value^,
}
XPT dr " dict row setter
'`key^': `value^,
XPT ds " dict setter
`var^['`key^'] = `value^
XPT sub " re.sub
re.sub(`pattern^, `replacement^, `source^)
XPT sea " re.search
re.search(`pattern^, `source^)
XPT tt " time.time
time.time()
XPT now " datetime.datetime.now
datetime.datetime.now()
