/*
Copyright (c) 2008 Kai Hendry (hendry.iki.fi)
Copyright (c) 2008 Cory Bennett (www.corybennett.org)

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

The Software shall be used for Good, not Evil.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

/* This is enhanced and modified code taken from Ian Bicking's blog post of
   2005-08-19 here:
   http://blog.ianbicking.org/jslint-command-line.html
*/

/* SpiderMonkey globals */
/*global environment, load, JSLINT, print, readline */

// wget http://www.jslint.com/fulljslint.js
load('/home/daethorian/config/vim/jslint/fulljslint.js');

var filename=arguments[0];
var body = "";
var line;
var blcount = 0;
while (true) {
  line = readline();
  body = body + line + "\n";
  // HACK
  // cant figure out how to tell EOF
  // from a normal blank line so arbitrarily
  // count 100 sequential blank lines and
  // assume we have finished

  if (typeof(line) != 'string') { break; }

  if (line.length === 0) {
    // blank line, so increment
    blcount = blcount + 1;
    if (blcount >= 100) {
      break;
    }
  }
  else {
    // not blank, so reset
    blcount = 0;
  }
}

// Good parts minus white space
boolOptions = {
	bitwise    : true, // if bitwise operators should not be allowed
	eqeqeq     : false, // if === should be required
	glovar     : true, // if HTML fragments should be allowed
	regexp     : true, // if the . should not be allowed in regexp literals
	undef      : false, // if variables should be declared before used
//	white      : true, // if strict whitespace rules apply
	browser    : false, // if the standard browser globals should be predefined
	plusplus   : false, // if increment/decrement should not be allowed
	nomen      : true // if names should be checked
};



var result = JSLINT(body, boolOptions);
if (!result) {
	for (i=0;i<JSLINT.errors.length;i++){
		obj = JSLINT.errors[i];
		// print ( obj.toSource() ); // DEBUG
		//print ( (obj["line"] + 1) + ":" + (obj["character"] + 1) + ":" + obj["reason"] );
        print ( filename + ":" + (obj['line']) + ":" + (obj['character']) + ":" + obj['reason'] );
	}
}
