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
	imcctran,
	skyctran,
	starfind,
	wcsctran,
	wcsedit,
	wcsreset	= "imcoords$x_images.e"

clbye()
