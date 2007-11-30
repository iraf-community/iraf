#{ IRRED -- KPNO IR Camera Reduction Package

# Load necessary core packages

images			# tasks sections,imcopy,imarith,imcombine,imdelete 
lists			# tokens task
utilities		# task translit
proto			# task bscale

# Define necessary paths

set	generic		= "noao$imred/generic/"
set	nproto		= "noao$nproto/"

package irred

task	irlincor	= "irred$x_irred.e"

task	iralign,
	irmatch1d,
	irmatch2d,
	irmosaic	= "nproto$x_nproto.e"

# Define the apphot centering and related tasks

task	center		= "irred$x_apphot.e"
task	centerpars	= "irred$centerpars.par"
task	datapars	= "irred$datapars.par"
task	txdump		= "irred$x_ptools.e"

# Scripts

task	flatten		= "generic$flatten.cl"
task	mosproc		= "irred$mosproc.cl"

clbye()
