#{ Package script task for the ONEDSPEC package.

# Define necessary paths

package onedspec

task	calibrate,
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
	scombine,
	sensfunc,
	sfit,
	sflip,
	sinterp,
	slist,
	specplot,
	specshift,
	splot,
	standard	= onedspec$x_onedspec.e

task	setairmass,
	setjd		= astutil$x_astutil.e

# Scripts and Psets

task	bplot		= onedspec$bplot.cl
task	ndprep		= onedspec$ndprep.cl
task	scopy		= onedspec$scopy.cl
task	rspectext	= onedspec$rspectext.cl
task	wspectext	= onedspec$wspectext.cl

task	$process	= process.cl		# Used by BATCHRED
task	dispcor1	= onedspec$dispcor1.par	# Used by DISPCOR
hidetask dispcor1,process,rstext

clbye
