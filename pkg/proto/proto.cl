#{ Package script task for the PROTO package.

images

package proto

task	binfil,
	bscale,
	epix,
	fields,
	fixpix,
	hfix,
	imcentroid,
	imcntr,
	imfunction,
	imreplace,
	imscale,
	interp,
	irafil,
	joinlines,
	suntoiraf,
	wcsedit,
	wcsreset	= proto$x_proto.e

task	imalign		= proto$imalign.cl

clbye
