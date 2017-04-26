#{ APEXTRACT -- Aperture extraction package

package apextract

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
	apnoise,
	apsum,
	aptrace		= "apextract$x_apextract.e"
task	apparams	= "apextract$apparams.par"
task	apall1		= "apextract$apall1.par"
task	apfit1		= "apextract$apfit1.par"
task	apflat1		= "apextract$apflat1.par"
task	apnorm1		= "apextract$apnorm1.par"
task	apnoise1	= "apextract$apnoise1.par"
task	apdefault	= "apextract$apdefault.par"
task	apscat1		= "apextract$apscat1.par"
task	apscat2		= "apextract$apscat2.par"

set	apdemos		= "apextract$apdemos/"
task	apdemos.pkg	= "apdemos$apdemos.cl"
 
hidetask apparams, apall1, apfit1, apflat1, apnorm1, apscat1, apscat2, apnoise1

clbye
