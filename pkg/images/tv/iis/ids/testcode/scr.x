# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "imd.h"
include <gset.h>
include <gki.h>

define	DIM	512
define	MCXSCALE	64
define	MCYSCALE	64

# scroll

procedure t_im()

pointer	gp
char	output[SZ_FNAME], output_file[SZ_FNAME], device[SZ_FNAME]
int	fd

pointer	gopen()
bool	streq()
int	open()
common	/local/gp

begin
	call clgstr("output", output, SZ_FNAME)
	if (!streq (output, "") ) {
	    call strcpy (output, output_file, SZ_FNAME)
	    fd = open (output_file, NEW_FILE, BINARY_FILE)
	} else
	    fd = open ("dev$stdimage", NEW_FILE, BINARY_FILE)

	call clgstr("device", device, SZ_FNAME)
	gp = gopen ( device, NEW_FILE, fd)

	call cl_button
	call scroll(0,0)
	call cursor(128,128)
	call wt_button
	call scroll(128,195)
	call cursor(128,128)
	call wt_button
	call zm(4,128,128)
	call wt_button
	call cursor(128,128)
	call wt_button
	call zm(1,205,205)

	# all done
	call gclose ( gp )
	call close ( fd )
end

procedure scroll(x,y)

int x,y

pointer	gp
common	/local/gp
short	data[8]

begin
	data[1] = IMD_SCROLL
	data[2] = IMD_WRITE
	data[3] = 2
	data[4] = IMD_EOD
	data[5] = IMD_EOD
	data[6] = 0
	data[7] = (x-1) * MCXSCALE
	data[8] = (y-1) * MCYSCALE
	call gescape(gp, IMD_CONTROL, data, 8)
end

procedure cursor(x,y)

int	x,y
pointer	gp
real	xr, yr
common	/local/gp

begin
	xr = real((x-1)*MCXSCALE)/GKI_MAXNDC
	yr = real((y-1)*MCXSCALE)/GKI_MAXNDC
	call gseti(gp, G_CURSOR, 1)
	call gscur(gp, xr, yr)
end

procedure wt_button

real	x,y
int	key
pointer	gp
common	/local/gp
begin
	call gseti(gp, G_CURSOR, IMD_BUT_WT)
	call ggcur(gp, x, y, key)
end

procedure cl_button

real	x,y
int	key
pointer	gp
common	/local/gp

begin
	call gseti(gp, G_CURSOR, IMD_BUT_RD)
	call ggcur(gp, x, y, key)
end

procedure zm(power, x,y)

int	power
int	x,y

short	data[9]
pointer	gp
common	/local/gp

begin
	data[1] = IMD_ZOOM
	data[2] = IMD_WRITE
	data[3] = 3
	data[4] = IMD_EOD
	data[5] = IMD_EOD
	data[6] = 0
	data[7] = power
	data[8] = (x-1) * MCXSCALE
	data[9] = (y-1) * MCYSCALE
	call gescape(gp, IMD_CONTROL, data, 9)
end
