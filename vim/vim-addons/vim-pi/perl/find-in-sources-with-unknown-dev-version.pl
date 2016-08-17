#!/usr/bin/perl
use strict;
use warnings;

use JSON;

#▶1 Help
if($ARGV[0] eq "--help") {
    print "Usage: $0 [--all] [--nomaisnrdeps] regex\n";
    print "  It uses db/scmsources.vim, vodb.json (dump of script-info)\n";
    print "    and ignore.lst file (file which contains list of numbers, \n";
    print "    each on its own line)\n";
    print "  --all will make script omit parsing db/scmsources.vim,\n";
    print "    but not ignore.lst file\n";
    print "  --nomaisnrdeps will make script also ignore plugins that have\n";
    print "    missing dependency information located in db/patch.vim\n";
    print "You must run this with current directory set to vim-pi root.\n";
    print "Do not forget to run\n";
    print "    curl 'http://www.vim.org/script-info.php' > vodb.json\n";
    print "before running this script unless you want make this script\n";
    print "download this file (requires libwww-perl).\n";
    exit 0;
}
#▶1 Define global constants
binmode STDOUT, ':utf8';
my $scmdb="db/scmsources.vim";
my $patchdb="db/patch.vim";
my $ignorelist="ignore.lst";
my $vimorg="http://www.vim.org";
my $vodburl="$vimorg/script-info.php";
#▶1 Define variables that depend on arguments
my $all          = ($ARGV[0] eq "--all"         ); shift @ARGV if $all;
my $nomaisnrdeps = ($ARGV[0] eq "--nomaisnrdeps"); shift @ARGV if $nomaisnrdeps;
my $regex=shift @ARGV; $regex=qr($regex);

my $sumreg=$regex;
my $descreg=$regex;
my $instreg=$regex;
#▶1 ignoreFromFile :: fname, varname, (ignoredsnrs :: {}) → + ignoredsnrs
sub ignoreFromFile {
    my ($fname, $varname, $ignoredsnrs)=@_;
    my $VIM;
    open $VIM, '<:utf8', $fname;
    while(<$VIM>) {
        next unless /^(?:" )?let $varname\.(\d+)\s*=/;
        $ignoredsnrs->{$1}=$_;
    }
    close $VIM;
}
#▶1 Populate ignoredsnrs
my %ignoredsnrs=();
ignoreFromFile($scmdb,   "scmnr",        \%ignoredsnrs) unless $all;
ignoreFromFile($patchdb, "mai_snr_deps", \%ignoredsnrs) if $nomaisnrdeps;
if(-e $ignorelist) {
    my $IGNORE;
    open $IGNORE, '<:utf8', $ignorelist;
    local $_;
    while(<$IGNORE>) {
        $ignoredsnrs{0+$_}=$_;
    }
    close $IGNORE;
}
#▶1 Get vim.org database
my %vo;
if(-e "vodb.json") {
    my $VODB;
    open $VODB, '<:utf8', "vodb.json";
    %vo=%{JSON::decode_json(join "", <$VODB>)};
    close $VODB;
}
else {
    use LWP::Simple;
    %vo=%{JSON::decode_json(LWP::Simple::get($vodburl))};
}
#▶1 PrintWithPrefix :: prefix, text → + stdout
sub PrintWithPrefix {
    my ($prefix, $text)=@_;
    print $prefix;
    $prefix=~s/./ /g;
    $text=~s/\r//g;
    local $_;
    my @lines=split /\n/, $text;
    return unless @lines;
    print (shift @lines);
    print "\n";
    for my $line (map {"$prefix$_"} @lines) {
        print "$line\n";
    }
}
#▶1 PrintSinfo :: sinfo → + stdout
sub PrintSinfo {
    my ($sinfo)=@_;
    print "Script type: ".$sinfo->{"script_type"}."\n";
    print "Author: ";
    print $sinfo->{"first_name"} if($sinfo->{"first_name"});
    print " " if($sinfo->{"first_name"} and $sinfo->{"last_name"});
    print $sinfo->{"last_name"} if($sinfo->{"last_name"});
    print "\n";
    print "Summary: ".$sinfo->{"summary"}."\n";
    PrintWithPrefix("Description: ", $sinfo->{"description"});
    PrintWithPrefix("Install details: ", $sinfo->{"install_details"});
}
#▲1
for my $snr (sort {$a<=>$b} (grep {not defined $ignoredsnrs{$_}} (keys %vo))) {
    my $sinfo=$vo{$snr};
    if(defined $sumreg and $sinfo->{"summary"} =~ $sumreg) {
        print "Summary matches for $snr:\n";
        PrintSinfo($sinfo);
    }
    elsif(defined $descreg and $sinfo->{"description"} =~ $descreg) {
        print "\nDescription matches for $snr:\n";
        PrintSinfo($sinfo);
    }
    elsif(defined $instreg and $sinfo->{"install_details"} =~ $instreg) {
        print "Installation details match for $snr:\n";
        PrintSinfo($sinfo);
    }
}
# vim: ft=perl tw=80 ts=4 sts=4 sw=4 et fmr=▶,▲
