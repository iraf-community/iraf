#{ Package script task for the ASTUTIL package.

package astutil

task	airmass,
	asthedit,
	precess,
	galactic,
	gratings,
	pdm,
	asttimes,
	rvcorrect,
	setairmass,
	setjd,
	ccdtime		= "astutil$x_astutil.e"

# PSET Tasks
task    keywpars        = "astutil$keywpars.par"

clbye
