#{ Package script task for the ASTUTIL package.

package astutil

# Compiled tasks.
task	airmass,
	astcalc,
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

# Script tasks.
task	astradius	= "astutil$astradius.cl"

# PSET tasks.
task    keywpars        = "astutil$keywpars.par"

clbye
