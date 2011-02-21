#{ Package script task for the UTILITIES package.

set 	nttools		= "utilities$nttools/"


package utilities

task	ucase,
	lcase,
	translit,
	detab,
	entab,
	urand,
	polyfit,
	curfit,
	surfit,
	split		= "utilities$x_utilities.e"

#  Utility scripts.
task    bases           = "utilities$bases.cl"

#  Sub-Packages.
task	nttools.pkg	= nttools$nttools.cl


# Load the NTTOOLS package when we are loaded.
nttools

clbye()
