#!/usr/bin/perl -w
# -*- perl -*-

use strict;

sub typesym {
  $_ = shift;
  s/\(.*\)//;
  tr/a-z/A-Z/;
  s/ /_/g;
  return $_;
}

$\ = "\n";

if (scalar(@ARGV) < 4) {
  print STDERR 'Too few arguments.';
  print STDERR 'usage: ./mathtestgen_mgr.pl function num-of-args arg1-type [arg2-type [...]] num-of-return-value ret1-type [ret2-type] <arguments...>';
  print STDERR 'examples:';
  print STDERR './mathtestgen_mgr.pl sin 1 double 1 double 0.0 M_PI/2.0 M_PI 3\*M_PI/2.0';
  print STDERR "./mathtestgen_mgr.pl atan2f 2 float float 1 float 1.0f '(float)sqrt(3.0)' 1.0f 1.0f";
  exit;
}

my $func = shift @ARGV;
my $nargs = shift @ARGV;
my @args;
for(my $i = 0; $i < $nargs ; $i++) {
  push @args, &typesym(shift @ARGV);
}
my $nrets = shift @ARGV;
my @rets;
my $ret2var;
for(my $i = 0; $i < $nrets ; $i++) {
  my $r = shift @ARGV;
  if (scalar(@rets) == 1 && $r =~ m!\((.*)\)!) {
    $ret2var = $1;
  }
  push @rets, &typesym($r);
}

open FH, ">$func.h";

print FH "#define TEST_FUNC $func";
print FH '';
print FH "#define ARG_TYPE $args[0]";
if ($nargs >= 2) {
  print FH "#define ARG2_TYPE $args[1]";
  if ($nargs >= 3) {
    print FH "#define ARG3_TYPE $args[2]";
  }
}
print FH "#define RET_TYPE $rets[0]";
if ($nrets >= 2) {
  print FH "#define RET2_TYPE $rets[1]";
  if (defined $ret2var) {
    print FH "#define RET2_VAR $ret2var";
  }
}
print FH '';
print FH '#define ARGS { \\';
$\ = '';
my $i = 0;
foreach (@ARGV) {
  if ($i % $nargs == 0) {
    print FH "  { ";
  }
  print FH $_;
  $i++;
  if ($i % $nargs == 0) {
    print FH " }, \\\n";
  } else {
    print FH ", ";
  }
}
$\ = "\n";
print FH '}';
