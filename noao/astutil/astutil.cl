#{ Package script task for the ASTUTIL package.

package astutil

task	airmass,
	precess,
	galactic,
	gratings,
	pdm,
	asttimes,
	rvcorrect,
	setairmass,
	setjd,
	ccdtime		= "astutil$x_astutil.e"

clbye
