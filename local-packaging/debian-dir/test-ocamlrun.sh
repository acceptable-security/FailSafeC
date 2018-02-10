#!/bin/sh
remove=false
if [ "$1" = "-r" ]; then
  remove=true
fi
for f in $@
do
    if [ -f "$f" ]; then
	if [ "`head -c2`" = '#!' ]; then
	    if [ $remove = true ]; then
		echo "file $f is refering ocamlrun; removing." 1>&2;
		rm "$f"
	    else
		echo "file $f is refering ocamlrun; rebuild it." 1>&2;
		exit 1
	    fi
	fi < "$f"
    else
	if [ $remove = false ]; then
	    echo "file $f missing" 1>&2;
	    exit 1
	fi
    fi
done
exit 0
