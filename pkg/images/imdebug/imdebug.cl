#{ IMDEBUG -- IMIO test and debug tasks.

set	imdebug	= "images$imdebug/"

package imdebug

task	$mkimage,
	$mktest,
	$cube,
	$maxmin,
	$gsubras,
	$dump		= "imdebug$x_imdebug.e"

clbye()
