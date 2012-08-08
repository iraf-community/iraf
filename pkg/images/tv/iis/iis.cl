plot

#{ IIS -- The IIS Image Display Control package.

package	iis

set	iis		= "images$tv/iis/"

task	cv,
	cvl		= "iis$x_iis.e"

task	blink		= "iis$blink.cl"
task	erase		= "iis$erase.cl"
task	$frame		= "iis$frame.cl"
task	lumatch		= "iis$lumatch.cl"
task	$monochrome	= "iis$monochrome.cl"
task	pseudocolor	= "iis$pseudocolor.cl"
task	rgb		= "iis$rgb.cl"
task	$window		= "iis$window.cl"
task	zoom		= "iis$zoom.cl"

clbye()
