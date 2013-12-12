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

XPT docstring " Triple-quoted docstring
"""
`cursor^

"""

..XPT

XPT ipdb " import ipdb
import ipdb ; ipdb.set_trace()

XPT pdb " import pdb
import pdb ; pdb.set_trace()

XPT . " self.
self.

XPT self " self
self

XPT assert " assert
assert `^

XPT test
def test_`function^(self):
    `cursor^

XPT setup
def setup_`method^(self`, method^):
    `cursor^

XPT teardown
def teardown_`method^(self`, method^):
    `cursor^


XPT raises wrap=job " with pytest.raises\()
with pytest.raises(`Exception^) as exc:
    `job^

XPT join
os.path.join(`^)
