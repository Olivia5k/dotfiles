#!/usr/bin/perl
use strict;
use warnings;
use JSON;
my %vo;
my $VODB;
open $VODB, '<:utf8', "db/vimorgsources.json";
%vo=%{JSON->new->relaxed(1)->decode(join "", <$VODB>)};
close $VODB;
my $key=shift @ARGV;
print join "\0", map {$_->{$key}} values %vo;
