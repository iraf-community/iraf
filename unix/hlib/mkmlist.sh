#!/bin/sh
# MKMLIST -- Make a library member list on the standard output, e.g., for
# inclusion in a MKPKG file.

_ml1() {
    ls	-- *.[xfcs]
    grep	'^include' -- *.x
}

_ml2() {
    _ml1 | grep -v '#' | grep -v '<syserr.h>' | sort | uniq |\
	sed -e	's/^.*include./ /' | sed -e 's/\"//g' |\
	sed -e	's/\.x/.x	/' | tr -s '\n ' '\t' 
}

printf "	"
_ml2 | sed -e 's/\(	\)\([^<]\)/#	\2/g' | tr '#' '\n' |\
    sed -e 's/>	</> </g'

