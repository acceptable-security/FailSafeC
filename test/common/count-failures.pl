#!/usr/bin/perl

use strict;
use FileHandle;

my $expected = $ARGV[0];

open EXP, $expected or die "can't open file";
my @exp_list = ();
while (<EXP>) {
  /^([^:]+):\s*(.*)$/;
  push @exp_list, [$1, $2];
}
close EXP;

my $c_ok = 0;
my $c_fail = 0;
my $c_unknown = 0;
my $c_unexp_ok = 0;
my $c_exp_fail = 0;
$| = 1;

process_unbuffered_input_by_lines
    (*STDIN,
     sub {
	chomp;
	/^(?:Compile (?:\.\.\/)*|Test )?([^:]+):\s*(.*)$/;
	my $pos = $1;
	my $oks = $2;
 	my $ok = $oks =~ /ok/i;
        my $unknown = $oks =~ /unknown/i;
	my @e = grep { $_->[0] eq $pos } @exp_list;
	if (@e) {
	    if ($ok) {
		$c_unexp_ok++;
		return "[[NOT EXPECTED]]";
	    } else {
		$c_exp_fail++;
		return "[[EXPECTED(" . $e[0]->[1] . ")]]";
	    }
	} else {
	    $c_ok++ if $ok;
            unless ($ok) {
              $c_unknown++ if $unknown;
              $c_fail++ unless $unknown;
            }
	    return "";
	}
    });

print "Ok $c_ok / Fail $c_fail / Unexpected Ok $c_unexp_ok / Expected Fail $c_exp_fail / Unknown $c_unknown\n";


sub process_unbuffered_input_by_lines {
    local *FH;
    my $subr;
    (*FH, $subr) = @_;
    my $buf = "";
    my ($lbuf, $len);
    while($len = sysread(FH, $buf, 512)) {
	last if ($len < 0);
	while(1) {
	    my $p = index($buf, "\n");
	    if ($p == -1) {
		print $buf;
		$lbuf .= $buf;
		last;
	    }
	    my $line = substr($buf, 0, $p);
	    print $line;
	    local $_ = ($lbuf . $line);
	    my $ret = &$subr($_);
	    undef $ret if (/ \Q$ret\E$/); # remove duplicate warnings in multi-pass processing
	    print (($ret ? " " : ""), $ret, "\n");
	    $lbuf = "";
	    $buf = substr($buf, $p + 1);
	    next;
	}
    }
}
