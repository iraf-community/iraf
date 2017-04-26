#{ IMGEOM -- The Image Geometric Transformation Package.

set	imgeom		= "images$imgeom/"

package	imgeom

# Tasks.

task	blkavg,
	blkrep,
	imshift,
	imtranspose,
	im3dtran,
	magnify,
	shiftlines	= "imgeom$x_images.e"

# Tasks in other packages 

# Geotran is used by the imlintran and rotate tasks.

task	geotran		= "immatch$x_images.e"
hidetask	geotran

# Scripts

task	imlintran	= "imgeom$imlintran.cl"
task	rotate		= "imgeom$rotate.cl"


clbye()
