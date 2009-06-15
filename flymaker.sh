#!/bin/bash


dname (){ (cd $1 ; pwd); }
bdname(){ basename `dname $1`; }

# if our module lives in a src dir, we attempt to handle the cases 
# .../lib/<app>/src/<our module>
# and
# .../<app>/src/<our module>
# assuming that include and ebin are parallel to src
Is=""
PAs=""

if [ `bdname $2` == "src" ]; then
    if [ `bdname $2/../..` == "lib" ]; then
        top=`dname $2/../..`
    else
        top=`dname $2/..`
    fi
    for i in $top/*/include; do
        Is="-I$i $Is"
    done
    for e in $top/*/ebin; do
        PAs="-pa$e $PAs"
    done
else
    top=`dname $2`
    PAs="-pa $top"
    Is="-I $top"
fi

OUT=`dirname $1`

erlc -o$OUT $Is $PAs -Wall -P $1 | grep -v "list comprehension has no gene"
exit 0
