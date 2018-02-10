AC_DEFUN([FSC_PERLIFY],
[
  $1=`perl -MData::Dumper -e '
    if (scalar(@ARGV) > 0) {
      @S|@Data::Dumper::Indent = 0;
      @S|@a = Dumper(\@ARGV);
      @S|@a =~ m/^.*?\@<:@(.*)\@:>@.*@S|@/;
      print @S|@1, ", ";
    }
    ' -- @S|@$2`
  AC_SUBST($1)
])
