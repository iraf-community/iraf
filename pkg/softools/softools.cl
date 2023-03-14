#{ SOFTOOLS -- Software tools package.  This package contains tools and
# utilities used by the programmer (or user-programmer) for software
# development.

package softools

task	$generic	= "$"//osfn("host$bin/generic.e")
task	$mkpkg		= "$"//osfn("host$bin/mkpkg.e")
task	$rmbin		= "$"//osfn("host$bin/rmbin.e")
task	$rmfiles	= "$"//osfn("host$bin/rmfiles.e")
task	$rtar		= "$"//osfn("host$bin/rtar.e")
task	$wtar		= "$"//osfn("host$bin/wtar.e")
task	$xc		= "$"//osfn("host$bin/xc.e")
task	$xyacc		= "$"//osfn("host$bin/xyacc.e")

task	mktags,
	mkapropos	= "softools$x_softools.e"

task	mkttydata	= "softools$x_mkttydata.e"

task	lroff,
	mkhelpdb,
	hdbexamine	= "system$x_system.e"

clbye()
