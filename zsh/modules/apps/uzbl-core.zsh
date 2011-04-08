function purge_cookies()
{
    d="$XDG_DATA_HOME/uzbl"
    for f in $d/cookies.txt $d/session-cookies.txt ; do
        fn=$(basename $f)
        cat $f | grep -v "$1" > /tmp/$fn
        mv /tmp/$fn $d
    done
}
