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

XPT doc " Triple-quoted docstring
"""
`docstring^

"""
`^

XPT ipdb " import ipdb
import ipdb ; ipdb.set_trace()

XPT . " self.
self.

XPT s " self
self

XPT test
def test_`function^(self):
    """
    `docstring^

    """

    `cursor^


XPT ae " self.assertEqual
self.assertEqual(`first^, `second^)

XPT at " self.assertTrue
self.assertTrue(`object^)

XPT af " self.assertFalse
self.assertFalse(`object^)

XPT ar " self.assertRaises
self.assertRaises(`Exception^, `method^`, `*args?^`, `**kwargs?^)

XPT as " Assert json success
self.assertTrue(j['success'])

XPT asf " Assert json failure
self.assertFalse(j['success'])

XPT cg " self.client.get
response = self.client.get(`url^, `get^)

XPT cp " self.client.post
response = self.client.post(`url^, `post^)

" cl is already taken by the class snippet
XPT login " self.client.login
self.client.login(username=`self.user.username^, password="nicke1")

XPT rj " Parse response to json
self.assertEqual(response.status_code, 200)
j = json.loads(response.content)


..XPT
