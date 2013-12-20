# vim

![wow screenshot](http://i.imgur.com/A8W8dYz.png)

Screenshot of a gvim session editing my dotfiles (because let's face it, that's
like 90% of what I do). Configuration is fully compatible with terminal vim,
with the only exception of things that bind on Mod1, but they have
corresponding `<leader>` mappings.

This vim setup is mostly used for hacking Python, Puppet, vimscript, web
development (HTML, CSS, javascript), and the occasional shellscript.

A complete list of plugins included can be found at the top of the vimrc file.
Plugins are managed using [vundle][vundle].


### Automagic

A few things will happen automagically:

* [delimitMate][delimitmate] will provide automatic closing of quotes,
  brackets, parentheses, etc.
* [vim-endwise][endwise] will provide the same for blocks, where applicable.
* [MatchTag][matchtag] will automatically highlight closing XML/HTML tags.
* [syntastic][syntastic] will check your file's syntax upon buffer write.
* Trailing whitespace, including dangling lines, will be deleted upon buffer
  write.
* Some mappings will reselect visual mode: `<` and `>` for indenting, and
  `<a-j>`/`<c-j>` or `<a-k>``<c-k>` for bubbling up and down, courtesy of
  [vim-unimpaired][unimpaired].
* Upon opening a buffer, it's `cwd` will be set to the corresponding VCS root.
  Non-VCS:d files will just be left alone with their current `pwd`.

The last one is important because it enables three cool mini-plugins to use the
`cwd` as base for input:

`<a-s>` (or `<leader>s`) will open a new [vimwiki][vimwiki] split based on the
current project's name. This can be used as your local todo for this project.
Invoking the mapping again will save and close the buffer.

`<a-a>` (or `<leader>a`) will invoke the `mor` binary (found in `utils/` in
this repository) for the current project name. This will net you a new terminal
with a tmux session set to the current project name. Use this for
terminal-related things to your project.

If a [virtualenv][virtualenv] is found, it's path is added to `$PATH`
automatically.


### Abbreviations

For convenience, a couple of abbreviations are included for common programming
symbol pairs. Some of them are easier and quicker to write in alphabetic form
rather than mucking around with shifted special characters. Just write the key
and then press `<space>` to activate the abbreviations.

* `eq`: `==`
* `ne`: `!=`
* `lt`: `<=`
* `gt`: `>=`
* `pe`: `+=` (`me` was not included since it's a common word)
* `hr`: `=>` (hashrocket!)


### Folds

As might be evident from the screenshot, I fucking love folds. They are the
best thing ever for navigating code, and it always bothers me that so few
people are enjoying them. Since I do that so much, I have mapped `<space>` to
be the same as `za`. If in gvim, one can also use `<c-space>` and `<s-space>`
to completely open or close folds.

If you're not already abusing folds, you really really should. They are better
for navigation then tags are.


### Python

This vim setup mostly sees Python, but no Python IDE features are to be found.
I've tried using [python-mode][python-mode] a couple of times but I only get
frustrated by how insanely slow it makes vim be. I also found that I disabled
mostly everything it does except [ropevim][ropevim] for tag navigation, and
I simply use a [sane tag setup][effortless] for that instead.

vim has very good builtin omnicompletion for Python. Run `<c-x><c-o>` in insert
mode and you'll start it. When you browse the completions, documentation for
the function will appear in a preview window.

I have written my own folding for Python: [vim-snakecharmer][snakecharmer]. It
is smarter than indent fold and knows about imports, classes, methods,
functions, different blocks and multiline expressions. It also knows about
decorators and comments, and only shows the line actually relevant, such as the
`def xyz()` line for methods.

I run tests with [pytest.vim][pytest] and check them with
[coveragepy.vim][coverage]. For non-py.test projects I usually run `nosetests`
inside of a [kicker][kicker] instance.

pytest.vim will run the test you are editing (the closest `def` above the
cursor) automatically when you save the test buffer. The following mappings are
available in Python test files:

* `ch`: `:Pytest previous`
* `cj`: `:Pytest previous && :Pytest next` (jump to current error)
* `ck`: `:Pytest error`
* `cl`: `:Pytest next`
* `cs`: `:Pytest session`
* `cm`: `:Pytest method`
* `cf`: `:Pytest file`

Lastly, I use the excellent little [switch.vim][switch] plugin to enable the
`-` key to do switching of keywords and statements. They map:

* `True` | `False`
* `==` | `!=`
* `in` | `not in`
* `setup` | `teardown`
* `def function(self)` | `def function()`
* `if/and/or/assert/is` | `if/and/or/assert/is not`
* Looping between `logging.x` methods (`debug`, `info`, `warning`, etc)

It might look weird at first glance, but they are all very useful.


### Git

I use git for everything, so I naturally spend some time with
[vim-fugitive][fugitive]. 90% of my git workflow is from inside of vim. To
speed all of that up, I have some *leaderless* mappings:

* `gs`: `:Gstatus`
* `gw`: `:Gwrite`
* `gc`: `:Gcommit`
* `gpp`: `:Git push origin `
* `guu`: `:Git pull --no-edit origin `

They are leaderless because I use them so often that I got tired of using the
leader. I never use any of the original binds anyway.

Also present are two very useful plugins for browsing history, either for the
repo ([gitv][gitv]) or for a single file ([vim-extradite][extradite]).

* `gll`: `:Gitv`
* `gle`: `:Extradite`


### Navigation

I use the wonderful [ctrlp.vim][ctrlp] for navigating files, buffer and tags.
It can be invoked via `<a-q>` (or `<leader>q`), which is quick and nice for the
hand. `<leader>a` can be used to browse buffers. I have no special
configurations or mappings other than an increased default height.

I also wrote [ctrlp-project][ctrlp-project] to navigate on a higher level. When
invoked (`<a-p>` or `<leader>z`) it will show a ctrlp selector listing all git
projects (and their submodules) within a set of predefined locations (`~/code`,
`~/git` etc). When a project is selected, a normal ctrlp will be spawned with
that project's root as starting point, allowing you to select a file to start
from.

Splits can be traversed with the common `<c-hjkl>` combo. New splits can be
created with `<c-z>` (horizontal) and `<c-s>` (vertical). The latter will
probably not work in your terminal.

Oh, and I kill windows so often I have mapped `K` to do so.

### Interface

The colorscheme in use is a slightly modified version of
[jellybeans.vim][jelly] that has more bolding emphasis and some color tweaks.

The statusline is the pure-vimscript [vim-airline][airline]. It has a slightly
modified jellybeans theme as well.

Also present for lighter use is the [github][github] colorscheme. I use it when
presenting something where a dark colorscheme simply does not work.


### ...and some thanks

...and finally, a hat tip to [Steve Losh][sjl] for making the vimrc I once upon
a time based all this off of.


<!--- Yay tabularize -->
[airline]:      https://github.com/bling/vim-airline
[coverage]:     https://github.com/alfredodeza/coveragepy.vim
[ctrlp-project]:https://github.com/thiderman/ctrlp-project
[ctrlp]:        https://github.com/kien/ctrlp.vim
[delimitmate]:  https://github.com/Raimondi/delimitMate
[endwise]:      https://github.com/tpope/vim-endwise
[extradite]:    https://github.com/int3/vim-extradite
[fugitive]:     https://github.com/tpope/vim-fugitive
[github]:       https://github.com/endel/vim-github-colorscheme
[gitv]:         https://github.com/gregsexton/gitv
[jelly]:        https://github.com/nanotech/jellybeans.vim
[kicker]:       https://github.com/alloy/kicker
[matchtag]:     https://github.com/gregsexton/MatchTag
[pytest]:       https://github.com/alfredodeza/pytest.vim
[python-mode]:  https://github.com/klen/python-mode
[ropevim]:      https://github.com/sontek/rope-vim
[snakecharmer]: https://github.com/thiderman/vim-snakecharmer
[switch]:       https://github.com/AndrewRadev/switch.vim
[syntastic]:    https://github.com/scrooloose/syntastic
[unimpaired]:   https://github.com/tpope/vim-unimpaired
[vimwiki]:      https://github.com/vim-scripts/vimwiki
[vundle]:       https://github.com/gmarik/vundle
[sjl]:          http://stevelosh.com/
[effortless]:   http://tbaggery.com/2011/08/08/effortless-ctags-with-git.html
[virtualenv]:   http://www.virtualenv.org/en/latest/
