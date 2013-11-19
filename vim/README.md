# vim

![wow screenshot](http://i.imgur.com/A8W8dYz.pngckk)

Screenshot of a gvim session editing my dotfiles (because let's face it, that's
like 90% of what I do). Configuration is fully compatible with terminal vim,
with the only exception of things that bind on Mod1, but they have
corresponding `<leader>` mappings.

This vim setup is mostly used for hacking Python, Puppet, vimscript, web
development (HTML, CSS, javascript), and the occasional shellscript.

A complete list of plugins included can be found at the top of the vimrc file.
Plugins are managed using [vundle][vundle].


## Automagic

A few things will happen automagically:

* [delimitMate][delimitmate] will provide automatic closing of quotes,
  brackets, parentheses, etc.
* [vim-endwise][endwise] will provide the same for blocks, where applicable.
* [MatchTag][matchtag] will automatically highlight closing XML/HTML tags.
* [syntastic][syntastic] will check your file's syntax upon buffer write.
* Trailing whitespace, including dangling lines, will be deleted upon buffer
  write.
* Upon opening a buffer, it's `cwd` will be set to the corresponding VCS root.
  Non-VCS:d files will just be left alone with their current `pwd`.

The last one is important because it enables two cool mini-plugins to use the
`cwd` as base for input:

`<a-s>` (or `<leader>s`) will open a new [vimwiki][vimwiki] split based on the
current project's name. This can be used as your local todo for this project.
Invoking the mapping again will save and close the buffer.

`<a-a>` (or `<leader>a`) will invoke the `mor` binary (found in `utils/` in
this repository) for the current project name. This will net you a new terminal
with a tmux session set to the current project name. Use this for
terminal-related things to your project.


## Folds

As might be evident from the screenshot, I fucking love folds. They are the
best thing ever for navigating code, and it always bothers me that so few
people are enjoying them. Since I do that so much, I have mapped `<space>` to
be the same as `za`. If in gvim, one can also use `<c-space>` and `<s-space>`
to completely open or close folds.

If you're not already abusing folds, you really really should. They are better
for navigation then tags are.


## Python

This vim setup mostly sees Python, but no Python IDE features are to be found.
I've tried using [python-mode][python-mode] a couple of times but I only get
frustrated by how insanely slow it makes vim be. I also found that I disabled
mostly everything it does except [ropevim][ropevim] for tag navigation, and
I simply use a [sane tag setup][effortless] for that instead.

I have written my own folding for Python: [vim-snakecharmer][snakecharmer]. It
is smarter than indent fold and knows about imports, classes, methods,
functions, different blocks and multiline expressions. It also knows about
decorators and comments, and only shows the line actually relevant, such as the
`def xyz()` line for methods.

I run tests with [pytest.vim][pytest] and check them with
[coveragepy.vim][coverage]. For non-py.test projects I usually run `nosetests`
inside of a [kicker][kicker] instance.

Lastly, I use the excellent little [switch.vim][switch] plugin to enable the
`-` key to do switching of keywords and statements. They map:

* True | False
* == | !=
* in | not in
* setUp | tearDown *(unittest/nose style)*
* setup_ | teardown_ *(py.test style)*
* def function(self) | def function()
* if/and/or/assert/is | if/and/or/assert/is not
* Looping between logging.x methods (debug, info, warning, etc)

It might look weird at first glance, but they are all pretty useful.


## Git

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


## Interface

The colorscheme in use is a slightly modified version of
[jellybeans.vim][jelly] that has more bolding emphasis and some color tweaks.

The statusline is the pure-vimscript [vim-airline][airline]. It has a slightly
modified jellybeans theme as well.

Also present for lighter use is the [github][github] colorscheme. I use it when
presenting something where a dark colorscheme simply does not work.


## ...and some thanks

...and finally, a hat tip to [Steve Losh][sjl] for making the vimrc I once upon
a time based all this off of.


<!--- Yay tabularize -->
[airline]:      https://github.com/bling/vim-airline
[coverage]:     https://github.com/alfredodeza/coveragepy.vim
[delimitmate]:  https://github.com/Raimondi/delimitMate
[endwise]:      https://github.com/tpope/vim-endwise
[extradite]:    https://github.com/int3/vim-extradite
[fugitive]:     https://github.com/tpope/vim-fugitive
[github]:       https://github.com/endel/vim-github-colorscheme
[gitv]:         https://github.com/gregsexton/MatchTag
[gitv]:         https://github.com/gregsexton/gitv
[jelly]:        https://github.com/nanotech/jellybeans.vim
[kicker]:       https://github.com/alloy/kicker
[pytest]:       https://github.com/alfredodeza/pytest.vim
[python-mode]:  https://github.com/klen/python-mode
[ropevim]:      https://github.com/sontek/rope-vim
[snakecharmer]: https://github.com/thiderman/vim-snakecharmer
[switch]:       https://github.com/AndrewRadev/switch.vim
[syntastic]:    https://github.com/scrooloose/syntastic
[vimwiki]:      https://github.com/vim-scripts/vimwiki
[vundle]:       https://github.com/gmarik/vundle
[sjl]:          http://stevelosh.com/
[effortless]:   http://tbaggery.com/2011/08/08/effortless-ctags-with-git.html
