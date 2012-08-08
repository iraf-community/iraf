# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<fio.h>
include	"fmio.h"

# FMIO_SETBUF -- Set the various buffer size parameters, making sure that the
# physical constraints are met, i.e., that the buffers are an integral
# multiple of the device block size, and do not exceed the maximum transfer
# size for the device.

procedure fmio_setbuf (fm)

pointer	fm			#I FMIO descriptor

int	devblksize, devmaxsize
int	optbufsize, maxbufsize, szbpage

begin
	# Get the device parameters for the device on which datafile resides.
	call zsttbf (FM_CHAN(fm), FSTT_BLKSIZE, devblksize)
	call zsttbf (FM_CHAN(fm), FSTT_MAXBUFSIZE, devmaxsize)

	# Make sure the page size is an integral multiple of the block size.
	szbpage = (FM_SZBPAGE(fm) + devblksize-1) / devblksize * devblksize

	# Set the optimum (default) file buffer size.
	optbufsize = FM_OPTBUFSIZE(fm)
	if (optbufsize <= 0) {
	    if (DEF_OPTBUFNP <= 0)
		call zsttbf (FM_CHAN(fm), FSTT_OPTBUFSIZE, optbufsize)
	    else
		optbufsize = DEF_OPTBUFNP * szbpage
	}

	# Set the maximum file buffer size.
	maxbufsize = FM_MAXBUFSIZE(fm)
	if (maxbufsize <= 0) {
	    if (DEF_MAXBUFNP > 0)
		maxbufsize = DEF_MAXBUFNP * szbpage
	    else
		maxbufsize = devmaxsize
	}

	# Apply constraints and store values.
	if (devmaxsize > 0)
	    maxbufsize = min (maxbufsize, devmaxsize)
	if (maxbufsize > 0)
	    FM_MAXBUFSIZE(fm) = max (szbpage, maxbufsize / szbpage * szbpage)

	FM_OPTBUFSIZE(fm) = max (szbpage, optbufsize / szbpage * szbpage)
	FM_DEVBLKSIZE(fm) = devblksize
	FM_SZBPAGE(fm)    = szbpage

	FM_DHMODIFIED(fm) = YES
end
