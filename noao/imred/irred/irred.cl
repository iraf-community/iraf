#{ IRRED -- KPNO IR Camera Reduction Package

# Load necessary packages

lists
utilities

# Define necessary paths

set	apphot		= "noao$digiphot/apphot/"
set	generic		= "noao$imred/generic/"
set	proto		= "noao$proto/"

package irred

task	bscale		= "irred$x_proto.e"

task	iralign,
	irmatch1d,
	irmatch2d,
	irmosaic	= "proto$x_proto.e"

# Define the apphot centering task

task	apselect	= "irred$x_apphot.e"
task	center		= "irred$x_apphot.e"
task	centerpars	= "irred$centerpars.par"
task	datapars	= "irred$datapars.par"

# Scripts

task	flatten		= "generic$flatten.cl"
task	mosproc		= "irred$mosproc.cl"

clbye()
