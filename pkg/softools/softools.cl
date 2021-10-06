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
	memchk,
	mkapropos	= "softools$x_softools.e"

task	mkttydata	= "softools$x_mkttydata.e"

task	lroff,
	mkhelpdb,
	hdbexamine	= "system$x_system.e"

clbye()
