#{ IMAGES package -- General image processing.

# Check that login.cl version matches IRAF version.  This has nothing to
# do with IMAGES, this is just a convenient place to test for an old login.cl,
# since IMAGES is virtually guaranteed to be loaded with IRAF.

if (cl.logver != cl.version) {
    print ("WARNING: login.cl version mismatch - rebuild with `mkiraf'")
    beep; sleep(1); beep; sleep(1); beep
}

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
	imexpr,
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
	xregister,
	_imaxes		= "images$x_images.e"

task	tv.pkg		= "tv$tv.cl"
task	imdebug.pkg	= "imdebug$imdebug.cl"
task	rotate		= "images$rotate.cl"
task	imlintran	= "images$imlintran.cl"
task	register	= "images$register.cl"

clbye()
