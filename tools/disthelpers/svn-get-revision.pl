#!/usr/bin/perl
use POSIX qw(strftime);

$ENV{'LANG'} = 'C';
$ENV{'LC_ALL'} = 'C';

open (F, "svn status -v . |") or die "cannot invoke svn: $!";

while (<F>) {
# 123456 1234567890 12345678 123456789012 
# A  +            -     1723 yutaka       local-packaging/debian-dir
    /(^[AMDR\+ \?]{6}) ([ \-\d]{10}) ([ \-\d\?]{8}) .{12} (..*)$/ or die "cannot parse svn output: \"$_\"\n ";
    $fname = $4;
    $frev = $3 + 0;
#    print $_;
    next if $1 =~ /D/;
    if ($1 =~ /[AMR+]/) {
	@stat = stat "$fname";
	if (@stat == 0) {
	    warn "cannot stat file $fname: $!";
	    next;
	}
	next if -d _;
#	printf "$fname: local change %s\n", strftime("%Y-%m-%d %H:%M:%S", (gmtime($stat[9])));
	if ($stat[9] > $lchange) {
	    $lchange = $stat[9];
	    $lchangefile = $fname;
	}
    } else {
#	printf "$fname: unchanged %d\n", $frev;
    }
    if ($rev < $frev) {
	$rev = $frev;
	$revchangefile = $fname;
    }
}
close F or die "error invoking svn: $!";

$revstr = $rev;
if ($lchange) {
    @tm = gmtime($lchange);
    $lchangedate = strftime('%Y%m%dT%H%M%SZ', @tm);
    $revstr = "$rev+$lchangedate";
}
if ($ARGV[0] eq "-v") {
    print STDERR "latest revision file is $revchangefile at $rev\n";
    print STDERR "with local modification $lchangefile at $lchangedate\n" if defined $lchangefile;
}
print "$revstr\n";
