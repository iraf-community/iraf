#{ SPECRED package definition

proto		# bscale

s1 = envget ("min_lenuserarea")
if (s1 == "")
    reset min_lenuserarea = 40000
else if (int (s1) < 40000)
    reset min_lenuserarea = 40000
 
package specred

# Slitproc
cl < doslit$doslittasks.cl
task	sparams		= "specred$sparams.par"

# Dofibers
task	dofibers	= "specred$dofibers.cl"
task	params		= "specred$params.par"

task	proc		= "srcfibers$proc.cl"
task	response	= "srcfibers$response.cl"
task	arcrefs		= "srcfibers$arcrefs.cl"
task	doarcs		= "srcfibers$doarcs.cl"
task	skysub		= "srcfibers$skysub.cl"
task	batch		= "srcfibers$batch.cl"
task	getspec		= "srcfibers$getspec.cl"
task	listonly	= "srcfibers$listonly.cl"
task	apscript	= "srcfibers$x_apextract.e"

# Generic fiber reduction tasks
task	msresp1d	= "specred$msresp1d.cl"

# Onedspec tasks
task	calibrate,
	continuum,
	deredden,
	dispcor,
	dopcor,
	fitprofs,
	identify,
	refspectra,
	reidentify,
	sapertures,
	sarith,
	scombine,
	sensfunc,
	sfit,
	slist,
	specplot,
	splot,
	standard	= "onedspec$x_onedspec.e"
task	bplot		= "onedspec$bplot.cl"
task	scopy		= "onedspec$scopy.cl"
task	dispcor1	= "onedspec$dispcor1.par"
 
# Apextract tasks
task	apall,
	apedit,
	apfind,
	apfit,
	apflatten,
	apmask,
	apnormalize,
	aprecenter,
	apresize,
	apscatter,
	apsum,
	aptrace		= "apextract$x_apextract.e"
task	apparams	= "apextract$apparams.par"
task	apall1		= "apextract$apall1.par"
task	apfit1		= "apextract$apfit1.par"
task	apflat1		= "apextract$apflat1.par"
task	apnorm1		= "apextract$apnorm1.par"
task	apdefault	= "apextract$apdefault.par"
task	apscat1		= "apextract$apscat1.par"
task	apscat2		= "apextract$apscat2.par"

# Astutil tasks
task	setairmass,
	setjd		= "astutil$x_astutil.e"

# Hide tasks from the user
hidetask apparams, apall1, apfit1, apflat1, apnorm1, apscat1, apscat2, dispcor1
hidetask sparams, params
hidetask apscript, proc, batch, arcrefs, doarcs, getspec, listonly, response

clbye
