# RMBIN -- Shell script to strip the binaries from the HSI.  This is normally
# done with the HSI bootstrap program 'rmbin', but this script can be used
# instead if the rmbin executable is not available for some reason.
#
#	USAGE:  'sh -x rmbin.sh'.

/bin/rm -f `find . \! -type d -print | grep '\.[aoe]$'`
