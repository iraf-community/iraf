#{ IMFILTER -- The Image Filtering Package.

set	imfilter	= "images$imfilter/"

package	imfilter

# Tasks.

task	boxcar,
	convolve,
	fmedian,
	fmode,
	frmedian,
	frmode,
	gauss,
	gradient,
	laplace,
	median,
	mode,
	rmedian,
	rmode,
	runmed		= "imfilter$x_images.e"

clbye()
