images
plot

#{ NOAO -- The NOAO optical astronomy suite of packages.

cl < "noao$lib/zzsetenv.def"
package	noao, bin = noaobin$

task	artdata.pkg	= "artdata$artdata.cl"
task	astrometry.pkg	= "astrometry$astrometry.cl"
task	astutil.pkg	= "astutil$astutil.cl"
task	digiphot.pkg	= "digiphot$digiphot.cl"
task	focas.pkg	= "focas$focas.cl"
task	imred.pkg	= "imred$imred.cl"
task	mtlocal.pkg	= "mtlocal$mtlocal.cl"
task	onedspec.pkg	= "onedspec$onedspec.cl"
task	proto.pkg	= "proto$proto.cl"
task	surfphot.pkg	= "surfphot$surfphot.cl"
task	twodspec.pkg	= "twodspec$twodspec.cl"

clbye()
