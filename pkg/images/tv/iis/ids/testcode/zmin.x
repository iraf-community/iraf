# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <error.h>
include <fio.h>
include <fset.h>
include "ids.h"
include <gki.h>
include <gset.h>

define	DIM	512
define	MCXSCALE	64
define	MCYSCALE	64

# zoom

procedure t_im()

pointer	gp
char	device[SZ_FNAME]

pointer	gopen()
int	dd[LEN_GKIDD]

short	i,data[DIM+1]
short	set_image[6]
int	key
real	x[30],y[30]
real	xjunk, yjunk

begin
	call clgstr("device", device, SZ_FNAME)
	call ids_open (device, dd)
	call gki_inline_kernel (STDIMAGE, dd)
	gp = gopen ( device, NEW_FILE, STDIMAGE)

	call fseti (STDIMAGE, F_TYPE, SPOOL_FILE)
	call fseti (STDIMAGE, F_CANCEL, OK)
	call ids_grstream (STDIMAGE)

	# now zoom after reading button presses
	# read first to clear box
	call gseti(gp, G_CURSOR, IDS_BUT_RD)
	call ggcur(gp, xjunk, yjunk, key)

	for ( i = 1 ; i < 5 ; i = i + 1) {
	    call gseti (gp, G_CURSOR, IDS_BUT_WT)
	    call ggcur(gp, xjunk, yjunk, key)
	    call gseti (gp, G_CURSOR, 1)
	    call rpc(gp, xjunk, yjunk, key)

	    data[11] = IDS_ZOOM
	    data[12] = IDS_WRITE
	    data[13] = 3
	    data[14] = IDS_EOD
	    data[15] = IDS_EOD
	    data[16] = 0
	    data[17] = 4
	    data[18] = min(((i-1)* 128) * MCXSCALE, GKI_MAXNDC)
	    data[19] = min(((i-1)* 128) * MCYSCALE, GKI_MAXNDC)
	    call gescape ( gp, IDS_CONTROL, data[11], 9)
	}

	# all done
	call gclose ( gp )
	call ids_close
end

# rpcursor --- read and print cursor

procedure rpc(gp, sx, sy, key)

pointer	gp
real	sx,sy
int 	key

begin
	call ggcur (gp, sx, sy, key)
	call eprintf("cursor: (%f,%f) (%d,%d) key %d\n")
	    call pargr (sx)
	    call pargr (sy)
	    call pargi ( int(sx*32767)/64)
	    call pargi ( int(sy*32767)/64)
	    call pargi (key)
end
