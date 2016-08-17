## VIM-PI (vim plugin index) (previously vim-addon-manager-known-repositories)

ZyX, Marc, Shougo are discussing how to move this all to the next level so that
all
- plugin manager (VAM, NeoBundle, PyPi etc)
- users
- distribution systems (Gentoo Portag, Nix, npackd)

can benefit the most.

We strongly welcome everybody who wants to contribute (also Pathogen and Vundle
and .. lovers).

## GOALS
- list all known Vim plugins
- do so in a way that everybody who is interested can reuse the information
- provide hints about deprecations or replacements
- collaborate with interested parties (other plugin managers etc)
- maybe cerate a website which provides advanced features
- setup standards for dependency management if possible.

## FUTURE
Marc Weber no longer thinks that it makes sense to create full
distribution/dependency/... tools for all things out there.
Instead he'll be working on a package manager managing multiple languages and
operating systems. A lot of additional issues cannot be solved in VimL, eg
supporting YouCompleteMe requires C/Python (thus Vim with Python support)
and so one. The [YPM](https://github.com/code-once/ypm) will try to fix this
all. Marc Weber will still maintain vim-pi and reply to requests.

You might also watch [neovim](github.com/neovim/neovim) which will focus on
lua. We have to wait to understand in which ways this will affect plugin
management for that Vim fork.

## SUPPORT
Open tickets, thanks

## important contents of this repository
- The database files: db/\*.json
- Manually maintained packages and patches db/\*.vim
- The Vim supporting code: autoload/\*.vim
  Suggested main entry ponit (default implementation): vam_known_repositories#Pool()
  It returns a list of all known plugins. Each plugin is a dictionary. An easy way to view
  contents is the :VAMPluginInfo command of VAM.
- python/\*.py files: scripts to update the database files
- doc/\*: additional documentation

If you're looking for code which actually knowns how to download the known plugins
we recommend VAM or NeoBundle.

### discussions going on:

#### discussion 1)
http://vim-wiki.mawercer.de/wiki/topic/vim%20plugin%20managment.html (bottom,
which hooks do we need, how to compile supporting tools, how declare dependencies)

#### discussion 2)

More discussions:
Some additional discussions are taking place at vim-pi's [bitbucket issue 
tracker](https://bitbucket.org/vimcommunity/vim-pi/issues).

- API, move contents to database having a online store ?: 
  https://bitbucket.org/vimcommunity/vim-pi/issue/80

- cross plugin manager way to describe plugins to be installed:
  https://bitbucket.org/vimcommunity/vim-pi/issue/95
  goals: allow external tools to install plugins (parallelization etc)


## projects using vim-pi
http://vim-plugins.org/plugin/?page=1

## Older vim-addon-manager-known-repository README contents:

This repository is an extension to http://github.com/MarcWeber/vim-addon-manager
(and NeoBundle).

It is the "default source" of descriptions where to get which addons for vim-addon-manager.

Have a look at all supported plugins online: http://mawercer.de/~marc/vam/index.php

For speed reasons vam#install#LoadKnownRepos() is used to load the repo as needed only.

As VAM this repository is mantainained by ZyX and Marc Weber

Contributing:

- Contribute to db/scmsources.vim which contains all the git,svn,mercurial,..
  sources

db/vimorgsources.json contains a dump from www.vim.org which is generated automatically.
The default implementation merges both prefering the source control ones

Issues which may cause us to immediately remove plugins†:

- security issues or other similar sever issues (didn't happen yet)

Issues which may cause us to deprecate plugins, which means they can be
installed but a warning will be shown†:

- there are other plugins doing the same, but better - unless the less useful 
  plugins are a significant simpler and people indiciate that
    they are still using it.

- a plugin is obviously broken in a way so that its causing more harm than 
  value.

† If in doubt create a bitbucket ticket and let's discuss the issue.

### BUGS:

  There are two plugins: one called align an done called Align. This will cause
  trouble on Windows !


# HISTORICAL INFO

**[VAM-KR][1] REPOSITORY IS NOW A MIRROR OF [VIM-PI][2]. YOU SHOULD POST ALL 
ISSUES AND PULL REQUESTS TO THE LATTER. ANY CHANGES TO THIS REPOSITORY RESULTING
IN A MERGE CONFLICT WITH UPSTREAM WILL BE DELETED BY A CRON JOB.**

[1]: https://github.com/MarcWeber/vim-addon-manager-known-repositories
[2]: https://bitbucket.org/vimcommunity/vim-pi

