#{ Package script task for the NPROTO package.

images			# for the script tasks findgain, findthresh, ndprep 

package nproto

task	binpairs,
	iralign,
	irmatch1d,
	irmatch2d,
	irmosaic,
	linpol,
	slitpic		= nproto$x_nproto.e

task	findgain	= nproto$findgain.cl
task	findthresh	= nproto$findthresh.cl

clbye
