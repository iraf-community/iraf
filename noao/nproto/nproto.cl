#{ Package script task for the NPROTO package.

images			# for the script tasks findthresh, ndprep 

package nproto

task	binpairs,
	iralign,
	irmatch1d,
	irmatch2d,
	irmosaic,
	linpol,
	slitpic		= nproto$x_nproto.e

task	findthresh	= "nproto$findthresh.cl"
task	mkms		= "nproto$mkms.cl"

task	detect		= "nproto$ace/x_nproto.e"
task	objmasks	= "nproto$ace/objmasks.cl"
task	objmasks1	= "nproto$ace/objmasks1.par"
hidetask detect, objmasks1

task	skysep		= "nproto$skysep.cl"
task	skygroup	= "nproto$skygroup.cl"

clbye
