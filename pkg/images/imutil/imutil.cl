#{ IMUTIL -- The Image Utilities Package.

set	imutil		= "images$imutil/"

package	imutil

# Tasks.

task	chpixtype,
	hedit,
	hselect,
	imarith,
	_imaxes,
	imcopy,
	imdelete,
	imdivide,
	imexpr,
	imfunction,
	imgets,
	imheader,
	imhistogram,
	imjoin,
	imrename,
	imreplace,
	imslice,
	imstack,
	imsum,
	imtile,
	imstatistics,
	listpixels,
	minmax,
	nhedit,
	sections	= "imutil$x_images.e"

clbye()
