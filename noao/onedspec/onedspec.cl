#{ Package script task for the ONEDSPEC package.

# Define necessary paths

package onedspec

task	autoidentify,
	calibrate,
	continuum,
	deredden,
	dispcor,
	disptrans,
	dopcor,
	fitprofs,
	identify,
	lcalib,
	mkspec,
	names,
	refspectra,
	reidentify,
	rstext,
	sapertures,
	sarith,
	sbands,
	odcombine,
	scoords,
	sensfunc,
	sfit,
	sflip,
	sinterp,
	skytweak,
	slist,
	specplot,
	specshift,
	splot,
	standard,
	telluric	= onedspec$x_onedspec.e

task	scombine	= "onedspec$scombine/x_scombine.e"

task	setairmass,
	setjd		= astutil$x_astutil.e

# Scripts and Psets

task	aidpars		= onedspec$aidpars.par
task	bplot		= onedspec$bplot.cl
task	ndprep		= onedspec$ndprep.cl
task	scopy		= onedspec$scopy.cl
task	rspectext	= onedspec$rspectext.cl
task	wspectext	= onedspec$wspectext.cl

task	$process	= process.cl		# Used by BATCHRED
task	dispcor1	= onedspec$dispcor1.par	# Used by DISPCOR
hidetask dispcor1,process,rstext

clbye
