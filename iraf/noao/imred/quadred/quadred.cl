#{ QUADRED -- QUAD CCD Reduction Package

set	ccddb	= "ccdred$ccddb/"

package quadred

# Special version of CCDPROC.

set	quadsrc		= "quadred$src/ccdproc/"

task	ccdproc		= quadsrc$x_quadred.e
task	qccdproc	= quad$x_ccdred.e

# Task from the CTIO QUAD package.

set	quad		= "quadred$src/quad/"

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
	gainmeasure,
	ccdgetparam		= "quad$x_quad.e"

task	quadproc		= "quad$quadproc.cl"
task	qproc			= "quad$qproc.cl"
task	qnoproc			= "quad$qnoproc.cl"
task	qstatistics		= "quad$qstatistics.cl"
task	qhistogram		= "quad$qhistogram.cl"

task	setinstrument		= "quad$setinstrument.cl"

hidetask ccdgetparam, ccddelete, ccdprcselect, ccdssselect, ccdsection
hidetask qpcalimage, qpselect, qproc, qnoproc, qccdproc

# Special versions which run quadproc rather than ccdproc
task	qdarkcombine	= quad$qdarkcombine.cl
task	qflatcombine	= quad$qflatcombine.cl
task	qzerocombine	= quad$qzerocombine.cl


# Tasks from the standard CCDRED package.

task	badpiximage,
	ccdgroups,
	ccdhedit,
	ccdinstrument,
	ccdlist,
	ccdmask,
	combine,
	mkfringecor,
	mkillumcor,
	mkillumflat,
	mkskycor,
	mkskyflat	= ccdred$x_ccdred.e

task	darkcombine	= ccdred$darkcombine.cl
task	flatcombine	= ccdred$flatcombine.cl
#task	setinstrument	= ccdred$setinstrument.cl
task	zerocombine	= ccdred$zerocombine.cl

clbye()
