# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "imd.h"
include <gki.h>
include <gset.h>

define	DIM	512
define	MCXSCALE	64
define	MCYSCALE	64

# zoom

procedure t_im()

pointer	gp
char	output[SZ_FNAME], output_file[SZ_FNAME], device[SZ_FNAME]
int	fd

pointer	gopen()
bool	streq()
int	open()

short	i,data[DIM+1]
short	set_image[6]
int	key
real	x[30],y[30]
int	xjunk, yjunk

begin
	call clgstr("output", output, SZ_FNAME)
	if (!streq (output, "") ) {
	    call strcpy (output, output_file, SZ_FNAME)
	    fd = open (output_file, NEW_FILE, BINARY_FILE)
	} else
	    fd = open ("dev$stdimage", NEW_FILE, BINARY_FILE)

	call clgstr("device", device, SZ_FNAME)
	gp = gopen ( device, NEW_FILE, fd)

	# now zoom after reading button presses
	# read first to clear box
	call gseti(gp, G_CURSOR, IMD_BUT_RD)
	call ggcur(gp, xjunk, yjunk, key)

	for ( i = 1 ; i < 5 ; i = i + 1) {
	    call gseti(gp, G_CURSOR, IMD_BUT_WT)
	    call ggcur(gp, xjunk, yjunk, key)

	    data[11] = IMD_ZOOM
	    data[12] = IMD_WRITE
	    data[13] = 3
	    data[14] = IMD_EOD
	    data[15] = IMD_EOD
	    data[16] = 0
	    data[17] = 4
	    data[18] = (((i-1)* 128)-1) * MCXSCALE
	    data[19] = (((i-1)* 128)-1) * MCYSCALE
	    call gescape ( gp, IMD_CONTROL, data[11], 9)
	}

	# all done
	call gclose ( gp )
	call close ( fd )
end
