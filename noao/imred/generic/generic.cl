#{ GENERIC -- Generic image reduction tools

# Load dependent packages:
images
proto			# Task "imreplace"

package generic

task	flat1d		= generic$x_generic.e

task	background	= generic$background.cl
task	darksub		= generic$darksub.cl
task	flatten		= generic$flatten.cl
task	normalize	= generic$normalize.cl
task	normflat	= generic$normflat.cl

clbye()
