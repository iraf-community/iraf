#{ LONGSLIT -- Longslit Package

# Load dependent packages

images		# Used in setimhdr

package longslit

set	generic		= "noao$imred/generic/"

# Tasks.

task	extinction,
	fitcoords,
	fluxcalib,
	illumination,
	response,
	transform	= longslit$x_longslit.e

task	identify,
	reidentify	= longslit$x_onedspec.e

task	background	= generic$background.cl

task	setairmass,
	setjd		= astutil$x_astutil.e

clbye
