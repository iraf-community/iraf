#! /bin/csh
# MKMLIST -- Make a library member list on the standard output, e.g., for
# inclusion in a MKPKG file.

# try to protect people from themselves...
unalias	ls ex rm grep sed sort uniq cat

ls	*.[xfcs] > _ml1
grep	'^include' *.x >> _ml1

grep -v	'#' _ml1 | grep -v '<syserr.h>' | sort | uniq |\
	sed -e	's/^.*include./ /' | sed -e 's/\"//g' |\
	sed -e	's/\.x/.x	/' > _ml2

ex - << 'EOC' _ml2
g/^ / .-1,.j
1,$s/^/	/
wq
'EOC'

cat	_ml2; rm _ml[12]
