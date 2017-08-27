#!/bin/bash

fakedb=/dev/shm/fakepacdb
realdb=/var/lib/pacman

[[ ! -d $fakedb ]] && { mkdir -p "$fakedb/sync" || exit 1; }
[[ ! -L $fakedb/local ]] && { ln -s "$realdb/local" "$fakedb" || exit 2; }

fakeroot pacman --dbpath "$fakedb" -Sy

installed=$(pacman -Q | wc -l)
new=$(pacman --dbpath "$fakedb" -Qqu | wc -l)

echo "$installed $new"
echo "$installed $new" > $fakedb/counts
