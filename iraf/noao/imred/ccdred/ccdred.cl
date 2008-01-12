#{ CCDRED -- CCD Reduction Package

set	ccddb	= "ccdred$ccddb/"
set	ccdtest	= "ccdred$ccdtest/"

package ccdred

task	$ccdtest	= ccdtest$ccdtest.cl

task	badpiximage,
	ccdgroups,
	ccdhedit,
	ccdinstrument,
	ccdlist,
	ccdmask,
	ccdproc,
	combine,
	mkfringecor,
	mkillumcor,
	mkillumflat,
	mkskycor,
	mkskyflat	= ccdred$x_ccdred.e

task	darkcombine	= ccdred$darkcombine.cl
task	flatcombine	= ccdred$flatcombine.cl
task	setinstrument	= ccdred$setinstrument.cl
task	zerocombine	= ccdred$zerocombine.cl

clbye()
