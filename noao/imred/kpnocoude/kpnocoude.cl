#{ KPNOCOUDE package definition

proto		# bscale

s1 = envget ("min_lenuserarea")
if (s1 == "")
    reset min_lenuserarea = 40000
else if (int (s1) < 40000)
    reset min_lenuserarea = 40000

# Define KPNOCOUDE package
package kpnocoude

set	demos		= "kpnocoude$demos/"

# Slitproc
cl < doslit$doslittasks.cl
task	sparams		= "kpnocoude$sparams.par"

# Dofibers
task	do3fiber	= "kpnocoude$do3fiber.cl"
task	params		= "kpnocoude$params.par"

task	proc		= "srcfibers$proc.cl"
task	response	= "srcfibers$response.cl"
task	arcrefs		= "srcfibers$arcrefs.cl"
task	doarcs		= "srcfibers$doarcs.cl"
task	skysub		= "srcfibers$skysub.cl"
task	batch		= "srcfibers$batch.cl"
task	getspec		= "srcfibers$getspec.cl"
task	listonly	= "srcfibers$listonly.cl"
task	mkfibers	= "srcfibers$mkfibers.cl"
task	apscript	= "srcfibers$x_apextract.e"

task	msresp1d	= "msred$msresp1d.cl"

# Onedspec tasks
task	continuum,
	deredden,
	dispcor,
	dopcor,
	refspectra,
	sapertures,
	sarith,
	scombine,
	slist,
	specplot,
	splot		= "onedspec$x_onedspec.e"
task	bplot		= "onedspec$bplot.cl"
task	scopy		= "onedspec$scopy.cl"
task	dispcor1	= "onedspec$dispcor1.par"

# Different default parameters
task	calibrate,
	identify,
	reidentify,
	sensfunc,
	standard	= "kpnocoude$x_onedspec.e"

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
hidetask mkfibers, params
hidetask apscript, proc, batch, arcrefs, doarcs, getspec, listonly, response

clbye
