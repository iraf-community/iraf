#{ HYDRA package definition

proto		# bscale

s1 = envget ("min_lenuserarea")
if (s1 == "")
    reset min_lenuserarea = 40000
else if (int (s1) < 40000)
    reset min_lenuserarea = 40000

# Define HYDRA package
package hydra

# Package script tasks
task	dohydra		= "hydra$dohydra.cl"
task	params		= "hydra$params.par"

# Fiber reduction script tasks
task	proc		= "srcfibers$proc.cl"
task	response	= "srcfibers$response.cl"
task	arcrefs		= "srcfibers$arcrefs.cl"
task	doarcs		= "srcfibers$doarcs.cl"
task	skysub		= "srcfibers$skysub.cl"
task	batch		= "srcfibers$batch.cl"
task	listonly	= "srcfibers$listonly.cl"
task	getspec		= "srcfibers$getspec.cl"

task	msresp1d	= "msred$msresp1d.cl"

# Demos
set	demos		= "hydra$demos/"
task	demos		= "demos$demos.cl"
task	mkfibers	= "srcfibers$mkfibers.cl"

# Onedspec tasks
task	continuum,
	dispcor,
	dopcor,
	identify,
	refspectra,
	reidentify,
	sapertures,
	sarith,
	scombine,
	slist,
	specplot,
	splot		= "onedspec$x_onedspec.e"
task	bplot		= "onedspec$bplot.cl"
task	scopy		= "onedspec$scopy.cl"
task	dispcor1	= "onedspec$dispcor1.par"

# Apextract tasks
task	apall,
	apedit,
	apfind,
	aprecenter,
	apresize,
	apscatter,
	apsum,
	aptrace		= "apextract$x_apextract.e"
task	apdefault	= "apextract$apdefault.par"
task	apparams	= "apextract$apparams.par"
task	apall1		= "apextract$apall1.par"
task	apscat1		= "apextract$apscat1.par"
task	apscat2		= "apextract$apscat2.par"
task	apscript	= "srcfibers$x_apextract.e"

# Astutil tasks
task	setairmass,
	setjd		= "astutil$x_astutil.e"

# Hide tasks from the user
hidetask apparams, apall1, apscript, apscat1, apscat2, dispcor1, mkfibers
hidetask params, proc, batch, arcrefs, doarcs, listonly, response, getspec

clbye()
