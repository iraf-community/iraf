#{ Package script task for the ASTUTIL package.

package astutil

task	airmass,
	precess,
	galactic,
	pdm,
	asttimes,
	rvcorrect,
	setairmass,
	ccdtime		= "astutil$x_astutil.e"

task	observatory	= imred$observatory.cl

clbye()
