#!/usr/bin/perl
use strict;
use warnings;

$ENV{"PERL_JSON_BACKEND"}="JSON::PP";
use Fcntl qw(:DEFAULT :flock);
use Time::HiRes;
use utf8;
use JSON;

#▶1 WL :: filehandle, line → + FS
sub WL {
    my ($F, $l)=@_;
    while(1) {
        flock $F, LOCK_EX and last;
        print STDERR "Lock failed";
        Time::HiRes::sleep(0.0625);
    }
    print $F $l;
    flush $F;
    flock $F, LOCK_UN
        or die "Failed to unlock file: $!";
}
#▲1
my $nrndbtarget="db/script-id-to-name-log.json";
my $NrNDB;
open $NrNDB, '<:utf8', $nrndbtarget;
my $nrndb=JSON->new()->utf8()->decode(join "", <$NrNDB>);
close $NrNDB;
open $NrNDB, '>:utf8', $nrndbtarget;
for my $key (keys %$nrndb) {
    my @newval=();
    my $prevwaserror=0;
    for my $name (@{$nrndb->{$key}}) {
        if(scalar @newval) {
            if($prevwaserror and $name eq $newval[-1]) {
                $prevwaserror=0;
                next;
            }
            elsif($name eq "error$key") {
                $prevwaserror=1;
                next;
            }
        }
        $prevwaserror=0;
        push @newval, $name;
    }
    if(scalar @newval and $newval[0] eq "error$key") {
        delete $nrndb->{$key};
    }
    else {
        $nrndb->{$key}=\@newval;
    }
}
my $nrjson=JSON->new()->utf8()
                      ->pretty()
                      ->indent(1)
                      ->sort_by(sub {$JSON::PP::a <=> $JSON::PP::b});
WL($NrNDB, $nrjson->encode($nrndb));
close $NrNDB;
# vim: ft=perl tw=80 ts=4 sts=4 sw=4 et fmr=▶,▲
