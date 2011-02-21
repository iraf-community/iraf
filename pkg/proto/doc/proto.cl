#{ Package script task for the PROTO package.

images

package proto

task	binfil,
	bscale,
	epix,
	fields,
	fixpix,
	hfix,
	imcntr,
	imextensions,
	imscale,
	interp,
	irafil,
	joinlines,
	$mask2text,
	mkglbhdr,
	mimstatistics,
	mskexpr,
	mskregions,
	suntoiraf,
	rskysub,
	text2mask 	= proto$x_proto.e

task	ringavg 	= proto$ringavg.cl

set	color		= "proto$color/"
set	vol		= "proto$vol/"

task	color.pkg	= color$color.cl
task	vol.pkg		= vol$vol.cl

hidetask	mask2text

clbye
