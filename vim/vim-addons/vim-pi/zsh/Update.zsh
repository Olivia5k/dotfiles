#!/bin/zsh
set -x
set -e
VIMPIREP=ssh://git@bitbucket.org/vimcommunity/vim-pi
VAMKRREP=ssh://git@github.com/MarcWeber/vim-addon-manager-known-repositories
VIMPIDIR=~/tmp/vim-pi
VIMPI_SAFE_REPO=~/.vam/vim-pi

GIT=/usr/bin/git
PYTHON=/usr/bin/python
PERL=/usr/bin/perl
CURL=/usr/bin/curl
RM=/bin/rm

integer committed=0

if ! test -d $VIMPIDIR ; then
    test -d $VIMPIDIR:h || /bin/mkdir -p $VIMPIDIR:h || exit 1
    $GIT clone --branch master $VIMPIREP $VIMPIDIR || exit 2
    cd $VIMPIDIR || exit 3
else
    cd $VIMPIDIR || exit 3
    $GIT reset --hard HEAD
    $GIT checkout --detach HEAD
    $GIT fetch -f $VIMPIREP master:master || exit 4
fi
$GIT checkout -f master || exit 5

function commit()
{
    if test -n "$($GIT status --porcelain ./db)" ; then
        if ! ( $GIT add ./db && $GIT commit ./db -m "Cron job update: $1" )
        then
            exit 15
        fi
        committed=1
    fi
}

$CURL -o script-info.json http://www.vim.org/script-info.php
if ! $PERL $VIMPI_SAFE_REPO/perl/www_vim_org.pl --verbose
then
    $RM script-info.json
    $GIT reset --hard HEAD
    exit 6
else
    commit "www_vim_org"
fi

if ! $PYTHON -O $VIMPI_SAFE_REPO/python/autoget.py new
then
    $RM script-info.json
    $GIT reset --hard HEAD
else
    commit "autoget"
fi
$RM script-info.json
if ! $PYTHON -O $VIMPI_SAFE_REPO/python/drchip.py ; then
    $GIT reset --hard HEAD
else
    commit "drchip"
fi

function safe_push()
{
    # By the time we are trying to push new changes may have added
    integer ATTEMPTS=5
    while ! $GIT push $@ master && (( ATTEMPTS-- )) ; do
        $GIT fetch -f $@ master:other-master
        $GIT checkout other-master
        $GIT merge --no-ff --commit -m "Merge master from $@"
        $GIT branch -D master
        $GIT branch -m master
    done
    (( ATTEMPTS != -1 ))
    return $?
}

if (( committed )) ; then
    if safe_push $VIMPIREP ; then
        # VAM-kr is a mirror now. Do not try too hard: 5 attempts or any merge 
        # conflict and “push -f”.
        safe_push $VAMKRREP || \
            $GIT push -f $VAMKRREP master
    fi
fi
