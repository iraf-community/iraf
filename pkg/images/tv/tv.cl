plot

#{ TV -- Image Display Control package.

package	tv

set	tv		= "images$tv/"
set	display		= "tv$display/"
set	cv		= "tv$cv/"

task	_dcontrol,
	display		= "display$x_display.e"

task	erase		= "tv$erase.cl"
task	blink		= "tv$blink.cl"
task	$frame		= "tv$frame.cl"
task	lumatch		= "tv$lumatch.cl"
task	$monochrome	= "tv$monochrome.cl"
task	pseudocolor	= "tv$pseudocolor.cl"
task	rgb		= "tv$rgb.cl"
task	$window		= "tv$window.cl"
task	zoom		= "tv$zoom.cl"

task	cv, cvl		= "cv$x_cv.e"

clbye()
