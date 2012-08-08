#{ SPECRED package definition

proto		# bscale

s1 = envget ("min_lenuserarea")
if (s1 == "")
    reset min_lenuserarea = 100000
else if (int (s1) < 100000)
    reset min_lenuserarea = 100000
 
package specred

# Slitproc
cl < doslit$doslittasks.cl
task	sparams		= "specred$sparams.par"

# Dofibers
task	dofibers	= "specred$dofibers.cl"
task	params		= "specred$params.par"

task	proc		= "srcfibers$proc.cl"
task	fibresponse	= "srcfibers$fibresponse.cl"
task	arcrefs		= "srcfibers$arcrefs.cl"
task	doarcs		= "srcfibers$doarcs.cl"
task	doalign		= "srcfibers$doalign.cl"
task	skysub		= "srcfibers$skysub.cl"
task	batch		= "srcfibers$batch.cl"
task	getspec		= "srcfibers$getspec.cl"
task	listonly	= "srcfibers$listonly.cl"
task	apscript	= "srcfibers$x_apextract.e"

# Generic fiber reduction tasks
task	msresp1d	= "specred$msresp1d.cl"

# Onedspec tasks
task	autoidentify,
	calibrate,
	continuum,
	deredden,
	dispcor,
	dopcor,
	fitprofs,
	identify,
	odcombine,
	refspectra,
	reidentify,
	sapertures,
	sarith,
	sensfunc,
	sfit,
	sflip,
	slist,
	skytweak,
	specplot,
	specshift,
	splot,
	standard,
	telluric	= "onedspec$x_onedspec.e"
task	scombine	= "onedspec$scombine/x_scombine.e"
task	aidpars		= "onedspec$aidpars.par"
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

# Longslit tasks
task	illumination,
	lscombine,
	response,
	transform	= "twodspec$longslit/x_longslit.e"
task	background	= "generic$background.cl"

# Astutil tasks
task	setairmass,
	setjd		= "astutil$x_astutil.e"

# Hide tasks from the user
hidetask apparams, apall1, apfit1, apflat1, apnorm1, apscat1, apscat2, dispcor1
hidetask sparams, params, doalign
hidetask apscript, proc, batch, arcrefs, doarcs, getspec, listonly, fibresponse

clbye
