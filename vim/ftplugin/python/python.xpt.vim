XPTemplate priority=personal

XPTvar $PYTHON_EXP_SYM ' as '
XPTvar $PYTHON_DOC_MARK '"""'
XPTvar $SParg      ' '


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

XPT rs " re.search
re.search(`pattern^, `source^)

XPT tt " time.time
time.time()

XPT now " datetime.datetime.now
datetime.datetime.now()

XPT docstring " Triple-quoted docstring
`$PYTHON_DOC_MARK^
`cursor^

`$PYTHON_DOC_MARK^

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

XPT join wrap=job " os.path.join\()
os.path.join(`job^)

XPT mp  " @mock.patch
@mock.patch(`^)

XPT mpo  " @mock.patch.object
@mock.patch.object(`^)

XPT nc " # pragma: nocover
# pragma: nocover

XPT staticmethod
staticmethod
