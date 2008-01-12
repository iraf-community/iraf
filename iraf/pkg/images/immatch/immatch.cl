#{ IMMATCH -- The Image Matching Package.

set	immatch		= "images$immatch/"
set	imgeom		= "images$imgeom/"
set	imutil		= "images$imutil/"

package	immatch

# Tasks.

task	imcentroid,
	imcombine,
	geomap,
	geotran,
	geoxytran,
	linmatch,
	psfmatch,
	skyxymatch,
	wcscopy,
	wcsxymatch,
	xregister,
	xyxymatch	= "immatch$x_images.e"

task	imshift		= "imgeom$x_images.e"
task	imcopy		= "imutil$x_images.e"
task	sections	= "imutil$x_images.e"
hidetask imshift, imcopy, sections

# Scripts

task	gregister	= "immatch$gregister.cl"
task	imalign		= "immatch$imalign.cl"
task	skymap		= "immatch$skymap.cl"
task	sregister	= "immatch$sregister.cl"
task	wcsmap		= "immatch$wcsmap.cl"
task	wregister	= "immatch$wregister.cl"


clbye()
