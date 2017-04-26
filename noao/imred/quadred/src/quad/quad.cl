#{ QUAD -- Quad CCD reduction package

noao
imred

set	ccddb			= "quad$ccddb/"
set	quadtest 		= "quad$quadtest/"

package quad

task	quadtest.pkg		= "quadtest$quadtest.cl"

task	quadsplit,
	quadjoin,
	quadscale,
	quadsections,
	ccddelete,
	ccdprcselect,
	ccdssselect,
	ccdsection,
	qpcalimage,
	qpselect,
	irlincor,
	gainmeasure,
#	ccdgetparam		= "quad$xx_quad.e"
	ccdgetparam		= "quad$x_quad.e"

task	quadproc		= "quad$quadproc.cl"
task	qproc			= "quad$qproc.cl"
task	qnoproc			= "quad$qnoproc.cl"
task	qstatistics		= "quad$qstatistics.cl"
task	qhistogram		= "quad$qhistogram.cl"

hidetask ccdgetparam, ccddelete, ccdprcselect, ccdssselect, ccdsection
hidetask qpcalimage, qpselect, qproc, qnoproc, quadsplit, quadjoin, quadsections

# CCDRED tasks.
task	badpiximage,
	ccdgroups,
	ccdhedit,
	ccdinstrument,
	ccdlist,
	combine,
	cosmicrays = ccdred$x_ccdred.e
#	cosmicrays,
#	mkfringecor,
#	mkillumcor,
#	mkillumflat,
#	mkskycor,
#	mkskyflat = ccdred$x_ccdred.e

task	setinstrument	= quad$setinstrument.cl

# Different default parameters
task	qccdproc	= quad$x_ccdred.e

# Special versions which run quadproc rather than ccdproc
task	darkcombine	= quad$darkcombine.cl
task	flatcombine	= quad$flatcombine.cl
task	zerocombine	= quad$zerocombine.cl

hidetask ccdproc

clbye()
