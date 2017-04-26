#{ VTEL -- Vacuum_telescope package.

# load necessary packages
images
tv

set	vtel		= "imred$vtel/"

package vtel

task	readvt,
	writevt,
	unwrap,
	quickfit,
	getsqib,
	putsqib,
	mscan,
	rmap,
	merge,
	destreak,
	trim,
	vtexamine,
	tcopy,
	pimtext,
	syndico,
	dicoplot	= "vtel$x_vtel.e"

# scripts

task	vtblink		= "vtel$vtblink.cl"
task	writetape	= "vtel$writetape.cl"
task	destreak5	= "vtel$destreak5.cl"
task	fitslogr	= "vtel$fitslogr.cl"
task	mrotlogr	= "vtel$mrotlogr.cl"
task	makeimages	= "vtel$makeimages.cl"
task	makehelium	= "vtel$makehelium.cl"

clbye()
