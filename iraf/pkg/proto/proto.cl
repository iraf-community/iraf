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
	mimstatistics,
	mskexpr,
	mskregions,
	suntoiraf,
	rskysub,
	text2mask = proto$x_proto.e

task	ringavg = proto$ringavg.cl

hidetask	mask2text

clbye
