#{ IMCOORDS -- The Image Coordinates Package.

set	imcoords	= "images$imcoords/"

package	imcoords

# Tasks.

task	ccfind,
	ccmap,
	ccsetwcs,
	cctran,
	ccxymatch,
	imcctran,
	skyctran,
	starfind,
	wcsctran,
	wcsedit,
	wcsreset	= "imcoords$x_images.e"

clbye()
