# QUADTEST -- QUAD Test package

# load artdata package (mknoise)
artdata

package quadtest

task	mkimage		= quadtest$x_ccdred.e
task	mkamp		= quadtest$mkamp.cl
task	mkquad		= quadtest$mkquad.cl
task	artobs		= quadtest$artobs.cl
task	ccdpars		= quadtest$ccdpars.par

clbye()
