#{ APEXTRACT -- Aperture extraction package

imred			# To get the tutor task

package apextract

task	apedit,
	apfind,
	apnormalize,
	apscatter,
	apstrip,
	apsum,
	aptrace		= "apextract$x_apextract.e"
task	apio		= "apextract$apio.cl"
task	apdefault	= "apextract$apdefault.cl"
task	apscat1		= "apextract$apscat1.par"
task	apscat2		= "apextract$apscat2.par"
 
hidetask apscat1, apscat2

clbye
