#{ IMCOORDS -- The Image Coordinates Package.

set	imcoords	= "images$imcoords/"

package	imcoords

# Tasks.

task	ccfind,
	ccget,
	ccmap,
	ccsetwcs,
	ccstd,
	cctran,
	ccxymatch,
	hpctran,
	imcctran,
	skyctran,
	starfind,
	wcsctran,
	wcsedit,
	wcsreset	= "imcoords$x_images.e"

task	mkcwcs		= "imcoords$src/mkcwcs.cl"
task	mkcwwcs		= "imcoords$src/mkcwwcs.cl"

clbye()
