#{ KPNOSLIT package definition

# Define KPNOSLIT package
package kpnoslit

set	demos		= "kpnoslit$demos/"

# Slitproc
cl < doslit$doslittasks.cl
task	sparams		= "kpnoslit$sparams.par"

# Onedspec tasks
task	continuum,
	deredden,
	dispcor,
	dopcor,
	identify,
	refspectra,
	reidentify,
	sarith,
	scombine,
	slist,
	splot,
	specplot	= "onedspec$x_onedspec.e"
task	bplot		= "onedspec$bplot.cl"
task	scopy		= "onedspec$scopy.cl"
task	dispcor1	= "onedspec$dispcor1.par"

# Different default parameters
task	calibrate,
	sensfunc,
	standard	= "kpnoslit$x_onedspec.e"

# Apextract tasks
task	apall,
	apedit,
	apfind,
	aprecenter,
	apresize,
	apsum,
	aptrace		= "apextract$x_apextract.e"
task	apdefault	= "apextract$apdefault.par"
task	apparams	= "apextract$apparams.par"
task	apall1		= "apextract$apall1.par"

# Demos
task	demos		= "demos$demos.cl"

# Astutil tasks
task	setairmass,
	setjd		= "astutil$x_astutil.e"

# Hide tasks from the user
hidetask apparams, apall1, dispcor1, sparams

clbye()
