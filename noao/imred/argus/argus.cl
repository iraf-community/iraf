#{ ARGUS package definition

proto		# bscale

s1 = envget ("min_lenuserarea")
if (s1 == "")
    reset min_lenuserarea = 100000
else if (int (s1) < 100000)
    reset min_lenuserarea = 100000

# Define ARGUS package
package argus

# Package script tasks
task	doargus		= "argus$doargus.cl"
task	params		= "argus$params.par"

# Fiber reduction script tasks
task	proc		= "srcfibers$proc.cl"
task	fibresponse	= "srcfibers$fibresponse.cl"
task	arcrefs		= "srcfibers$arcrefs.cl"
task	doarcs		= "srcfibers$doarcs.cl"
task	doalign		= "srcfibers$doalign.cl"
task	skysub		= "srcfibers$skysub.cl"
task	batch		= "srcfibers$batch.cl"
task	listonly	= "srcfibers$listonly.cl"
task	getspec		= "srcfibers$getspec.cl"

task	msresp1d	= "specred$msresp1d.cl"

# Demos
set	demos		= "argus$demos/"
task	demos		= "demos$demos.cl"
task	mkfibers	= "srcfibers$mkfibers.cl"

# Onedspec tasks
task	autoidentify,
	continuum,
	dispcor,
	dopcor,
	identify,
	refspectra,
	reidentify,
	sapertures,
	sarith,
	sflip,
	slist,
	specplot,
	specshift,
	splot		= "onedspec$x_onedspec.e"
task	scombine	= "onedspec$scombine/x_scombine.e"
task	aidpars		= "onedspec$aidpars.par"
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
task	apscript	= "srcfibers$x_apextract.e"
task	apscat1		= "apextract$apscat1.par"
task	apscat2		= "apextract$apscat2.par"

# Astutil tasks
task	setairmass,
	setjd		= "astutil$x_astutil.e"

# Hide tasks from the user
hidetask apparams, apall1, apscript, apscat1, apscat2, dispcor1, mkfibers
hidetask params, proc, batch, arcrefs, doarcs, listonly, fibresponse, getspec
hidetask doalign

clbye()
