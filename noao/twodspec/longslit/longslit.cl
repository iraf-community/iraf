#{ LONGSLIT -- Longslit Package

# Load dependent packages

images		# Used in setimhdr

package longslit

set	generic		= "noao$imred/generic/"
set	onedstds	= "noao$lib/onedstds/"

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

clbye
