#{ CTIOSLIT package definition

# Define CTIOSLIT package
package ctioslit

set	demos		= "ctioslit$demos/"

# Slitproc
cl < doslit$doslittasks.cl
task	sparams		= "ctioslit$sparams.par"

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
	standard	= "ctioslit$x_onedspec.e"

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

# Astutil tasks
task	setairmass,
	setjd		= "astutil$x_astutil.e"

# Demos
task	demos		= "demos$demos.cl"

# Hide tasks from the user
hidetask apparams, apall1, dispcor1, sparams

clbye()
