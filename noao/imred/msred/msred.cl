#{ MSRED package definition
 
set	onedstds	= "noao$lib/onedstds/"
 
package msred
 
# Onedspec tasks
task	calibrate,
	identify,
	msdispcor,
	msselect,
	refspectra,
	reidentify,
	sensfunc,
	setdisp,
	specplot,
	splot,
	standard	= "msred$x_onedspec.e"
task	msreidentify	= "msred$msreidentify.cl"
task	msbplot		= "msred$msbplot.cl"
task	dispcor1	= "msred$dispcor1.par"
 
# Apextract tasks
task	apedit,
	apfind,
	apnormalize,
	apscatter,
	apstrip,
	apsum,
	aptrace		= "msred$x_apextract.e"
task	apio		= "msred$apio.par"
task	apscat1		= "msred$apscat1.par"
task	apscat2		= "msred$apscat2.par"
task	apdefault	= "msred$apdefault.par"
 
# Hide tasks from the user
hidetask apio, apscat1, apscat2, apstrip, dispcor1
 
clbye()

