#{ IMAGES package -- General image processing.

set	imdebug		= "images$imdebug/"
set	tv		= "images$tv/"

package	images

task	blkavg,
	blkrep,
	boxcar,
	chpixtype,
	convolve,
	fit1d,
	fmedian,
	fmode,
	gauss,
	geomap,
	geotran,
	gradient,
	hedit,
	hselect,
	imarith,
	imcombine,
	imcopy,
	imdelete,
	imdivide,
	imheader,
	imhistogram,
	imgets,
	imrename,
	imshift,
	imslice,
	imstack,
	imstatistics,
	imsum,
	imsurfit,
	imtranspose,
	laplace,
	lineclean,
	listpixels,
	magnify,
	median,
	minmax,
	mode,
	sections,
	shiftlines,
	_imaxes		= "images$x_images.e"

task	tv.pkg		= "tv$tv.cl"
task	imdebug.pkg	= "imdebug$imdebug.cl"
task	rotate		= "images$rotate.cl"
task	imlintran	= "images$imlintran.cl"
task	register	= "images$register.cl"

clbye()
