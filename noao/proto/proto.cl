#{ Package script task for the PROTO package.

tv

package proto


task	epix,
	fixpix,
	fields,
	imcentroid,
	imcntr,
	imedit,
	imexamine,
	imfunction,
	interp,
	imreplace,
	imscale,
	imslice,
	imstack,
	imtitle,
	iralign,
	irmatch1d,
	irmatch2d,
	irmosaic,
	join,
	mkhistogram,
	radplt,
	slitpic,
	toonedspec,
	tvmark,
	binpairs,
	binfil,
	bscale,
	irafil 		= proto$x_proto.e

task	ndprep		= proto$ndprep.cl
task	imalign		= proto$imalign.cl

# Imexamine psets.
task	cimexam = proto$cimexam.par;	hidetask cimexam
task	eimexam = proto$eimexam.par;	hidetask eimexam
task	himexam = proto$himexam.par;	hidetask himexam
task	limexam = proto$limexam.par;	hidetask limexam
task	rimexam = proto$rimexam.par;	hidetask rimexam
task	simexam = proto$simexam.par;	hidetask simexam
task	vimexam = proto$vimexam.par;	hidetask vimexam

keep
clbye()
