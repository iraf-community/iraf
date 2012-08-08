#{ NOAO -- The NOAO optical astronomy suite of packages.

cl < "noao$lib/zzsetenv.def"
package	noao, bin = noaobin$

task	artdata.pkg	= "artdata$artdata.cl"
task	astrometry.pkg	= "astrometry$astrometry.cl"
task	astcat.pkg	= "astcat$astcat.cl"
task	astutil.pkg	= "astutil$astutil.cl"
task	digiphot.pkg	= "digiphot$digiphot.cl"
task	focas.pkg	= "focas$focas.cl"
task	imred.pkg	= "imred$imred.cl"
task	mtlocal.pkg	= "mtlocal$mtlocal.cl"
task	nobsolete.pkg	= "nobsolete$nobsolete.cl"
task	nproto.pkg	= "nproto$nproto.cl"
task	obsutil.pkg	= "obsutil$obsutil.cl"
task	onedspec.pkg	= "onedspec$onedspec.cl"
task	rv.pkg		= "rv$rv.cl"
task	surfphot.pkg	= "surfphot$surfphot.cl"
task	twodspec.pkg	= "twodspec$twodspec.cl"

task	observatory	=  "astutil$x_astutil.e"

clbye()
