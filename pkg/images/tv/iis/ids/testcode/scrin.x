# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <error.h>
include <fio.h>
include <fset.h>
include "ids.h"
include <gset.h>
include <gki.h>

define	DIM	512
define	MCXSCALE	64
define	MCYSCALE	64

# scroll

procedure t_im()

pointer	gp
char	device[SZ_FNAME]

pointer	gopen()
int	dd[LEN_GKIDD]
common	/local/gp

begin
	call clgstr("device", device, SZ_FNAME)
	call ids_open (device, dd)
	call gki_inline_kernel (STDIMAGE, dd)
	gp = gopen ( device, NEW_FILE, STDIMAGE)

	call fseti (STDIMAGE, F_TYPE, SPOOL_FILE)
	call fseti (STDIMAGE, F_CANCEL, OK)
	call ids_grstream (STDIMAGE)

	call cl_button
	call scroll(1,1)
	call cursor(129,129)
	call wt_button
	call scroll(129,195)
	call cursor(129,129)
	call wt_button
	call zm(4,129,129)
	call wt_button
	call cursor(129,129)
	call wt_button
	call zm(1,205,205)

	# all done
	call gclose ( gp )
	call ids_close
end

procedure scroll(x,y)

int x,y

pointer	gp
common	/local/gp
short	data[8]

begin
	data[1] = IDS_SCROLL
	data[2] = IDS_WRITE
	data[3] = 2
	data[4] = IDS_EOD
	data[5] = IDS_EOD
	data[6] = 0
	data[7] = (x-1) * MCXSCALE
	data[8] = (y-1) * MCYSCALE
	call gescape(gp, IDS_CONTROL, data, 8)
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
	call gseti(gp, G_CURSOR, IDS_BUT_WT)
	call ggcur(gp, x, y, key)
end

procedure cl_button

real	x,y
int	key
pointer	gp
common	/local/gp

begin
	call gseti(gp, G_CURSOR, IDS_BUT_RD)
	call ggcur(gp, x, y, key)
end

procedure zm(power, x,y)

int	power
int	x,y

short	data[9]
pointer	gp
common	/local/gp

begin
	data[1] = IDS_ZOOM
	data[2] = IDS_WRITE
	data[3] = 3
	data[4] = IDS_EOD
	data[5] = IDS_EOD
	data[6] = 0
	data[7] = power
	data[8] = (x-1) * MCXSCALE
	data[9] = (y-1) * MCYSCALE
	call gescape(gp, IDS_CONTROL, data, 9)
end
