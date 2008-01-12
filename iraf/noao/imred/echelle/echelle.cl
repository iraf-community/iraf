#{ ECHELLE -- Echelle Spectral Reduction Package

# Load necessary packages
proto		# bscale

# Increase header space for echelle format keywords
s1 = envget ("min_lenuserarea")
if (s1 == "")
    reset min_lenuserarea = 100000
else if (int (s1) < 100000)
    reset min_lenuserarea = 100000

package echelle

# Ecslitproc and dofoe
cl < doecslit$slittasks.cl
cl < dofoe$dofoetasks.cl

# Demos
set	demos		= "echelle$demos/"
task	demos		= "demos$demos.cl"

# Onedspec tasks
task	continuum,
	deredden,
	dispcor,
	dopcor,
	ecidentify,
	ecreidentify,
	refspectra,
	sapertures,
	sarith,
	sflip,
	slist,
	specplot,
	specshift,
	splot		= "onedspec$x_onedspec.e"
task	scombine	= "onedspec$scombine/x_scombine.e"
task	bplot		= "onedspec$bplot.cl"
task	scopy		= "onedspec$scopy.cl"
task	dispcor1	= "onedspec$dispcor1.par"

# Different default parameters
task	calibrate,
	sensfunc,
	standard	= "echelle$x_onedspec.e"

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
hidetask apparams, apall1, apfit1, apflat1, apnorm1
hidetask apscat1, apscat2, dispcor1

# Set echelle extraction output
apall.format = "echelle"
apsum.format = "echelle"

clbye
