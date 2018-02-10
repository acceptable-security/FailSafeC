#!/usr/bin/perl
# -*- perl -*-

use Data::Dumper;
use File::Basename;
use strict 'vars';

use constant SECTION => '__failsafeC_typeinfo';

our $srcdir = (dirname($0) || ".");

sub read_manifest_dump_entry {
    local (*FP) = @_;
    
    my $line = <FP>;

    return undef if !defined($line);
    
    die unless $line =~ /^([a-z---]+):(\d+)\n$/;
    my $s;
    read(FP, $s, $2);
    die unless length $s == $2;
    die unless <FP> eq "\n";
#    print STDERR "got $1\n";
    return ($1, $s);
}

sub parse_archive_manifest {
    my ($fname) = @_;
    my @result;

#    print STDERR "parsing $fname\n";

    open(ELF, '-|') || exec '../../tools/read-elf-section', '-S', SECTION, '-M', '--optional', $fname;

    my ($tag, $str) = read_manifest_dump_entry(*ELF);

    die unless $tag eq 'archive-name';
    die unless $str eq $fname;

    ($tag, $str) = read_manifest_dump_entry(*ELF);

    while (defined $tag) {
	die unless $tag = 'file-name';
	my $fname = $str;

	($tag, $str) = read_manifest_dump_entry(*ELF);
	
	next unless $tag eq 'section';
	die unless $str eq SECTION;

	($tag, $str) = read_manifest_dump_entry(*ELF);
	die unless $tag eq 'contents';

	chop $str if substr($str, -1, 1) eq "\000";
	chop $str if substr($str, -1, 1) eq "\n";

	push @result, $fname, $str;
#	print STDERR "got entry for $fname\n";

	($tag, $str) = read_manifest_dump_entry(*ELF);
    }

    close ELF or die "error reading results from read-elf-section: $?";

    return @result;
}

our ($stdlib_ec, $fscw_ec);

{
    -f "$srcdir/src.manifest.in" or die "cannot find $srcdir/src.manifest: $!";
    local $/;
    open (STDLIB_IN, "$srcdir/src.manifest.in") or die "cannot open $srcdir/src.manifest: $!";
    $stdlib_ec = <STDLIB_IN>;
    $stdlib_ec =~ s/[ \t]*\#\#[^\n]*\n/\n/g;
    $stdlib_ec =~ s/\n\n+/\n/g;
    $stdlib_ec =~ s/\A\n//g;
    $stdlib_ec =~ s/\n\z//g;
    close (STDLIB_IN) or die "error reading results from ec: $?";
    chop $stdlib_ec if substr($stdlib_ec, -1, 1) eq "\n";

    open (FSCW_EC, "fscw/fscw.manifest") or die 'cannot open fscw.manifest';
    $fscw_ec = 
	<FSCW_EC>;
    chop $fscw_ec if substr($fscw_ec, -1, 1) eq "\n";
    close (FSCW_EC);
}

our %archive_manifests = parse_archive_manifest($ARGV[0]);

make_analyzed_manifest(\%archive_manifests, $ARGV[0], $stdlib_ec . "\n" . $fscw_ec);

#$archive_manifests{'<src>'} = $stdlib_ec . "\n" . $fscw_ec;

#$archive_manifests{'<fscw>'} = $fscw_ec;

dump_manifest($ARGV[0], \%archive_manifests);

sub print_entry {
    my ($tag, $str) = @_;
    printf "%s:%d\n%s\n", $tag, length $str, $str;
}

sub dump_manifest {
    my $fname = $_[0];
    my %archive_manifests = %{$_[1]};

    print_entry('archive-name', $fname);
    foreach my $e (sort keys %archive_manifests) {
	print_entry('file-name', $e);
	print_entry('section', SECTION);
	print_entry('contents', $archive_manifests{$e});
    }
}

sub make_analyzed_manifest {
    our %known_utility_modules = ('stdlib_util.opt.o' => 1);

    my ($archive_manifests, $fname, $src_manifest) = @_;

#    $archive_manifests->{'<src>'} = $src_manifest;

    my @src_manifest = split "\n", $src_manifest;

    my $R_line = shift @src_manifest;

    my (%defvals, %typeinfos, %defvals_notused);
    my %refnames;

    my @builtins;

    foreach (@src_manifest) {
	if (/^([DE])\t([\w\d]+)\t([\w\d]+)(\t.*)?$/) {
	    my ($def_p, $sym, $typ, $add) = ($1, $2, $3, $4);
	    die "$2 duplicated in manifest file" if exists $defvals{$2};
	    $defvals{$2} = [$3,$1,$4];
	    $refnames{"$3_$2"} = $2;
	    if ($sym =~ /^__builtin_/) {
		push @builtins, $_;
	    } else {
		$defvals_notused{$sym} = 1 if $def_p eq 'D';
	    }
	} elsif (/^TSS\t([\w\d]+)\t([\w\d]+)$/) {
	    die "$1 duplicated in manifest file" if exists $typeinfos{$1};
	    $typeinfos{$1} = $2;
	    $refnames{"fsc_typeinfo_$1"} = $2;
#	    push @builtins, $_;
	} elsif (/^TI\t/) {
	    #ignored
	} else {
	    die "unknown line $_";
	}
    }
    
    open (I, "nm $fname |") or die "cannot open $fname: $!";

    my ($current_fname, $current_active_p);
    my (@out_ts, @out_ti, @out_d, @out_e);
    my $symbols_p;

#    print STDERR "existing keys:\n>>", join("\n>>", keys %refnames), "\n";
    $archive_manifests->{'<builtins>'} = join("\n", ($R_line . "\tForce-linking"), @builtins);

    while(1) {
	$_ = <I>;
	if (eof(I) || /^$/) {
	    if ($current_active_p) {
		my $manifest = join("\n", $R_line, @out_ts, @out_ti, @out_d, @out_e);
		if (@out_ts && $manifest =~ /^TSS\t/m) {
#		    print STDERR "notice: module $current_fname contains TSS entry: force emitting\n";
		    $manifest =~ s/^(R\t.*)?$/$1\tForce-linking/m;
		    $archive_manifests->{$current_fname} = $manifest;
		} elsif (@out_d) {
		    $archive_manifests->{$current_fname} = $manifest;
		} elsif ($symbols_p) {
		    if (@out_e || @out_ti) {
			warn "there is an external FSC dependency without FSC symbols in $current_fname.\n";
		    } else {
			print STDERR "notice: no FSC symbols in $current_fname; may be a utility module.\n"
			    unless $known_utility_modules{$current_fname};
		    }
		} else {
		    print STDERR "notice: $current_fname is empty\n";
		}
	    };
	    $current_active_p = 0;
	    $current_fname = '';
	    @out_ts = @out_ti = @out_d = @out_e = ();
	    $symbols_p = 0;
	    last if eof(I);
	    next;
	} elsif (/^([\w.\/]+):$/) {
	    die if $current_fname;
	    $current_fname = $1;
	    if (exists $archive_manifests->{$1}) {
#		print STDERR "$1 is already contained.\n";
		$current_active_p = 0;
	    } else {
#		print STDERR "$1 is new.\n";
		$current_active_p = 1;
	    }
	} elsif (/^[0-9a-f]* +([DTU]) (\w+)$/) {
	    $symbols_p = 1;
	    die if $current_fname eq '';
	    my ($type, $name) = ($1, $2);
	    if ($name =~ /^(GV|FSP?|fsc_typeinfo)_(..*)/) {
		my ($deftype, $defval) = ($1, $2);
		# external reference
		if ($deftype eq 'fsc_typeinfo') {
		    if (exists $typeinfos{$defval}) {
			push @out_ts, "${\ ($type eq 'U' ? 'TSA' : 'TSS')}\t$defval\t$typeinfos{$defval}";
		    }
		    push @out_ti, "TI\t$defval" if $type eq 'U' && $defval !~ /^X(ud|ti)_$/;
		} else {
		    if ($current_active_p) {
			# files covered in src.manifest
			if (exists $refnames{$defval}) {
			    my $sym = $refnames{$defval};
			    my @t = @{$defvals{$sym}};
#			    print STDERR ">>>>$sym($type): @t\n";
			    if ($type eq 'U') {
				push @out_e, "E\t$sym\t$t[0]$t[2]";
			    } else {
				if ($defvals_notused{$sym}) {
				    push @out_d, "D\t$sym\t$t[0]$t[2]";
				    $defvals_notused{$sym} = 0;
				} else {
				    die "$sym duplicated within library";
				}
			    }				
			} else {
			    die "ERROR\t$type $name\t$defval\tnot found";
			}
		    } else {
			# files covered without src.manifest
			if (exists $refnames{$defval}) {
			    my $sym = $refnames{$defval};
			    my @t = @{$defvals{$sym}};
#			    print STDERR ">>>>$sym($type): @t\n";
			    if ($type ne 'U' && $t[1] eq 'D') {
				warn "$sym($type) is defined in $current_fname, but contained in src.manifest";
			    }
			}
		    }
		}
	    }
	}
    }

    foreach (keys %defvals) {
	warn "$_ not defined in any module inside library" if $defvals_notused{$_};
    }
    
    close (I) or die "cannot close $fname: $!/$@";
}

#make_analyzed_manifest(\%archive_manifests, $ARGV[0], $stdlib_ec . "\n" . $fscw_ec);
