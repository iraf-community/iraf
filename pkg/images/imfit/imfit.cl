#{ IMFIT -- The Image Fitting Package.

set	imfit		= "images$imfit/"

package	imfit

# Tasks.

task	fit1d,
	imsurfit,
	lineclean	= "imfit$x_images.e"

clbye()
