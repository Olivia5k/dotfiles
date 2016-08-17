#!/usr/bin/perl
# www_vim_org.pl fetches current set of known plugins and sources from www.vim.org
# It updates db/vimorgsources.json and db/script-id-to-name-log.json

use strict;
use warnings;

$ENV{"PERL_JSON_BACKEND"}="JSON::PP";

use LWP::UserAgent;
use Fcntl qw(:DEFAULT :flock);
use Time::HiRes;
use utf8;
use JSON;
use HTML::Entities;

my $verbose=0;
if($ARGV[0] eq "--verbose") {
    $verbose=1;
    shift @ARGV;
}

my $ua=LWP::UserAgent->new(cookie_jar => {},
                             max_size => 100*1024*1024,
                    protocols_allowed => ['http'],
                              timeout => 30,
                                agent => "www_vim_org.pl");

my $vimorg="http://www.vim.org";
my $base="$vimorg/scripts";
my $maxattempts=3;
my $vodbtarget="db/vimorgsources.json";
my $nrndbtarget="db/script-id-to-name-log.json";

my %children;

#▶1 get :: url → response
sub get {
    my ($url)=@_;
    my $attempt = 0;
    my $response;
    while($attempt<$maxattempts) {
        $response=$ua->get($url);
        return $response if($response->is_success);
        $attempt++;
        print STDERR "Failed to get $url, attempt $attempt";
    }
    die "Failed to get $url";
}
#▶1 WL :: filehandle, line → + FS
sub WL {
    my ($F, $l)=@_;
    while(1) {
        flock $F, LOCK_EX and last;
        print STDERR "Lock failed";
        Time::HiRes::sleep(0.0625);
    }
    print $F $l;
    eval {flush $F;};
    flock $F, LOCK_UN
        or die "Failed to unlock file: $!";
}
#▶1 copyScalar :: a → a
sub copyScalar {
    my ($a)=@_;
    return $a;
}
#▶1 addToDct :: dict, key, value → + dict
sub addToDct {
    my ($dict, $key, $value)=@_;
    if(not defined $dict->{$key}) {
        $dict->{$key}=[$value]; }
    elsif(copyScalar($dict->{$key}[0]) ne copyScalar($value)) {
        unshift @{$dict->{$key}}, $value; }
}
#▶1 formatScripts :: [script], fh, fh, fh → + FS: scripts.yaml, scripts.dat, db/
sub formatScripts {
    my ($scripts)=@_;
    my ($VODB, $NrNDB, $nrndb) = openDBs();
    WL($VODB, "{\n");
    my $json=JSON->new()->canonical();
    for my $script (@$scripts) {
        my $lastsrc=$script->{"sources"}->[0];
        my $snr=$script->{"snr"};
        my $sid=$script->{"id"};
        addToDct($nrndb, $snr, $sid);
        WL($VODB, '"'.$script->{"id"}.'":'.
                  $json->encode({"script-type" => $script->{"type"},
                                 vim_script_nr => $script->{"snr"},
                                       version => $lastsrc->{"version"},
                                  archive_name => $lastsrc->{"archive"},
                                           url => "$base/download_script.php?src_id=".$lastsrc->{"srcnr"},
                                          type => "archive"}).",\n");
    }
    WL($VODB, "}");
    my $nrjson=JSON->new()->utf8()
                          ->pretty()
                          ->indent(1)
                          ->sort_by(sub {$JSON::PP::a <=> $JSON::PP::b});
    WL($NrNDB, $nrjson->encode($nrndb));
}
#▶1 openDBs :: () → FD + …
sub openDBs() {
    my $VODB;
    open $VODB, '>:utf8', $vodbtarget
        or die $!;
    my $NrNDB;
    open $NrNDB, '<:utf8', $nrndbtarget;
    my $nrndb=JSON->new()->utf8()->decode(join "", <$NrNDB>);
    close $NrNDB;
    open $NrNDB, '>:utf8', $nrndbtarget;
    return ($VODB, $NrNDB, $nrndb);
}
#▶1 genName :: name, snr, scriptnames → sname + scriptnames
sub genName {
    local $_;
    my ($_, $snr, $scriptnames)=@_;
    s/\.vim$//g;
    # XXX That must purge at least ' and \n
    s/[^ a-zA-Z0-9_\-.]//g;
    s/ /_/g;
    while(defined $scriptnames->{$_}) {
        my $s=$scriptnames->{$_};
        if(ref $s) {
            $s->{"id"}.="%".$s->{"snr"};
            $scriptnames->{$s->{"id"}}=$s;
            $scriptnames->{$_}=1;
        }
        $_.="\%$snr";
    }
    return $_;
}
#▶1 addScriptID :: [script] → + [script]
sub addScriptID {
    my ($scripts)=@_;
    local $_;
    my $scriptnames={};
    for my $script (@$scripts) {
        $script->{"id"}=genName($script->{"name"}, $script->{"snr"},
                                $scriptnames);
        $scriptnames->{$script->{"id"}}=$script;
    }
}
#▶1 getAllScripts
sub getAllScripts() {
    my $file = "script-info.json";
    my $jstring;
    if(-e $file) {
        print "Using file $file\n" if($verbose);
        my $F;
        open $F, '<:utf8', $file;
        $jstring=join "", <$F>;
    }
    else {
        my $url="$vimorg/script-info.php";
        print "Processing $url\n" if($verbose);
        $jstring=get($url)->decoded_content();
    }
    my $json;
    eval {$json=JSON->new()->utf8()->decode($jstring)};
    unless(defined $json) {
        die "Failed to parse json: $@";
    }
    local $_;
    my $scripts=[sort {$b->{"snr"} <=> $a->{"snr"}}
                      (map {{snr => 0+$_->{"script_id"},
                            name => decode_entities($_->{"script_name"}),
                            type => $_->{"script_type"},
                         sources => [map {{srcnr => +$_->{"src_id"},
                                         archive => decode_entities($_->{"package"}),
                                         version => decode_entities($_->{"script_version"}),
                                     vim_version => decode_entities($_->{"vim_version"}),}}
                                         (sort {$b->{"creation_date"} <=>
                                                $a->{"creation_date"}}
                                               @{$_->{"releases"}})],
                            }} values %$json)];
    addScriptID($scripts);
    formatScripts($scripts);
    return;
}
getAllScripts();
# vim: ft=perl tw=80 ts=4 sts=4 sw=4 et fmr=▶,▲
