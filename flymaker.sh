#!/usr/bin/env bash

dname (){ (cd $1 ; pwd); }
bdname(){ basename `dname $1`; }

# if our module lives here;
# .../a/b/<our module>
# we add these to include/load paths, respectively;
# .../*/include
# .../*/ebin
# note: the load path is used to resolve the -include_lib()"
Is=""
PAs=""

# $HOME/bla.erl
if [ `dname $2` == "$HOME" ]; then echo -n ""

# $HOME/bla/foo.erl or /bla/foo.erl
elif [ `bdname $2/..` == "$HOME" -o `bdname $2/..` == "/" ]; then  echo -n ""

# $HOME/bla/src/foo.erl or /bla/src/foo.erl
elif [ `bdname $2/../..` == "HOME" -o `bdname $2/../..` == "/" ]; then
    top=`dname $2/..`
    Is="-I $top/include"
    PAs="-pa $top/ebin"

# $HOME/lib/app/src/foo.erl or /erlang/app/src/foo.erl
else
    top=`dname $2/../..`
    for i in $top/*/include; do
        Is="-I$i $Is"
    done
    for e in $top/*/ebin; do
        PAs="-pa$e $PAs"
    done
fi

OUT=`dirname $1`

erlc -o $OUT $Is $PAs -Wall $1 | grep -v "list comprehension has no gene"

exit 0
