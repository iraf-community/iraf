#{ ARTDATA - Artificial data package

package artdata

task	gallist,
	mk1dspec,
	mk2dspec,
	mkechelle,
	mkheader,
	mknoise,
	mkobjects,
	mkpattern,
	starlist	= "artdata$x_artdata.e"

set	mkexamples	= "artdata$mkexamples/"
task	mkexamples	= "mkexamples$mkexamples.cl"

clbye()
