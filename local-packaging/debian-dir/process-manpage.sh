#!/usr/bin/perl
open (I, "<", $ARGV[0]) or die;
open (O, ">", $ARGV[1]) or die;

while (defined ($_ = <I>)) {
    s@/path/to/fscc@fscc@g;
    s@/usr/local/lib/fsc/@/usr/lib/fsc/@g;
    s@^.I LICENSE$@.I /usr/share/doc/failsafec/copyright@g;
    print O;
}
