#{ SOFTOOLS -- Software tools package.  This package contains tools and
# utilities used by the programmer (or user-programmer) for software
# development.

package softools

task	$generic,
	$mkpkg,
	$rmbin,
	$rmfiles,
	$rtar,
	$wtar,
	$xc,
	$xyacc		= "$foreign"

task	mktags,
	memchk		= "softools$x_softools.e"

task	mkttydata	= "softools$x_mkttydata.e"
task	mkmanpage	= "softools$mkmanpage.cl"

task	lroff,
	mkhelpdb,
	hdbexamine	= "system$x_system.e"

clbye()
