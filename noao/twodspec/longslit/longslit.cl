#{ LONGSLIT -- Longslit Package

# Load dependent packages

images		# Used in setimhdr

package longslit

set	generic		= "noao$imred/generic/"
set	demos		= "longslit$demos/"

# Tasks.

task	extinction,
	fceval,
	fitcoords,
	fluxcalib,
	illumination,
	lscombine,
	response,
	transform	= longslit$x_longslit.e

task	calibrate,
	reidentify,
	sensfunc,
	standard	= longslit$x_onedspec.e

task	autoidentify,
	deredden,
	dopcor,
	identify,
	lcalib,
	sarith,
	sflip,
	slist,
	specplot,
	specshift,
	splot		 = onedspec$x_onedspec.e

task	aidpars		= onedspec$aidpars.par
task	bplot		= onedspec$bplot.cl
task	scopy		= onedspec$scopy.cl

task	background	= generic$background.cl

task	setairmass,
	setjd		= astutil$x_astutil.e

# Demos
task	demos		= demos$demos.cl

hidetask slist

clbye
