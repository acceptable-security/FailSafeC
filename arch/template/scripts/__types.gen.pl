#!/usr/bin/perl -w
# -*- perl -*-

use strict;

my @includes = qw(
  sys/types.h
  stddef.h stdint.h unistd.h time.h
  termios.h
  sys/socket.h sys/resource.h
  );

my %types = (
  size_t      => [[qw(unsigned integer)], 'size of objects.'],
  ssize_t     => [[qw(signed integer)],   'size of objects and error indication.'],
  ptrdiff_t   => [[qw(signed integer)],   'difference of two pointers.'],

  off_t       => [[qw(signed integer)],   'file offset.'],

  time_t      => [[qw(integer)],          'time in seconds.'],
  clock_t     => [[qw(integer)],          'clock ticks.'],
  suseconds_t => [[qw(integer)],          'time in microseconds.'],
  useconds_t  => [[qw(integer)],          'time in microseconds.'],

  int8_t      => [[qw(signed integer)],   'exact 8bit signed integer.'],
  int16_t     => [[qw(signed integer)],   'exact 16bit signed integer.'],
  int32_t     => [[qw(signed integer)],   'exact 32bit signed integer.'],
  int64_t     => [[qw(signed integer)],   'exact 64bit signed integer.'],
  uint8_t     => [[qw(unsigned integer)], 'exact 8bit unsigned integer.'],
  uint16_t    => [[qw(unsigned integer)], 'exact 16bit unsigned integer.'],
  uint32_t    => [[qw(unsigned integer)], 'exact 32bit unsigned integer.'],
  uint64_t    => [[qw(unsigned integer)], 'exact 64bit unsigned integer.'],
  intptr_t    => [[qw(signed integer)],   'signed integer type enough to hold pointer to void.'],
  uintptr_t   => [[qw(unsigned integer)], 'unsigned integer type enough to hold pointer to void.'],
  intmax_t    => [[qw(signed integer)],   'greatest signed integer type.'],
  uintmax_t   => [[qw(unsigned integer)], 'greatest unsigned integer type.'],

  socklen_t   => [[qw(integer)],          'length of sockaddr.'],
  sa_family_t => [[qw(unsigned integer)], 'socket address family.'],

  id_t        => [[qw(integer)],          'id.'],
  uid_t       => [[qw(integer)],          'user id.'],
  gid_t       => [[qw(integer)],          'group id.'],
  pid_t       => [[qw(integer)],          'process id.'],

  dev_t       => [[qw(integer)],          'device id.'],
  ino_t       => [[qw(integer)],          'inode number.'],
  mode_t      => [[qw(integer)],          'file mode.'],
  blkcnt_t    => [[qw(integer)],          'block count.'],
  blksize_t   => [[qw(integer)],          'block size.'],
  nlink_t     => [[qw(integer)],          'link count'],

  fsblkcnt_t  => [[qw(integer)],          'file system block count.'],
  fsfilcnt_t  => [[qw(integer)],          'file system file count.'],

  rlim_t      => [[qw(unsigned integer)], 'resource limit value.'],

  cc_t        => [[qw(integer)],          'special character.'],
  speed_t     => [[qw(integer)],          'terminal baud rate.'],
  tcflag_t    => [[qw(integer)],          'terminal mode.'],
);

my @inttypes = ('char', 'short', 'int', 'long', 'long long');
my @floattypes = ('float', 'double', 'long double');

$\ = "\n";

print '<%#cflags -D_XOPEN_SOURCE=600 #%>';
print '<%#shared /* -*- c -*- */';

foreach (@includes) {
  print "#include <$_>";
}

print '#%>';

print '/* <%=s EC_VERSION_MSG %> */';

print '#ifndef ____TYPES_H__';
print '#define ____TYPES_H__';
print '';
print '#include <_stddef.h>';

foreach(sort keys %types){
  my $t = $_;
  print '';
  print '/**';
  print ' * @brief ', @{$types{$t}}[1];
  print ' */';

  my %f = map { $_ => 1 } @{@{$types{$t}}[0]};
  my @c;
  if(($f{signed} || !$f{unsigned}) && ($f{integer} || $f{arithmetic})){
    @c = (@c, map { 'signed '.$_} @inttypes);
  }
  if(($f{unsigned} || !$f{signed}) && ($f{integer} || $f{arithmetic})){
    @c = (@c, map { 'unsigned '.$_} @inttypes);
  }
  if(($f{signed} || !$f{unsigned}) && ($f{'floating-point'} || $f{arithmetic})){
    @c = (@c, @floattypes);
  }

  print map {
    "<%[[?%><%# $t a; $_ a;#%>typedef $_ __$t;<%]];%>"
    } @c;
}

print '';
print '#endif';
