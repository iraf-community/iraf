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

short	i, data[DIM+1]
int	key, but, fnum
real	x, y
real	xjunk, yjunk

begin
	call clgstr("device", device, SZ_FNAME)
	call ids_open (device, dd)
	call gki_inline_kernel (STDIMAGE, dd)
	gp = gopen ( device, NEW_FILE, STDIMAGE)

	call fseti (STDIMAGE, F_TYPE, SPOOL_FILE)
	call fseti (STDIMAGE, F_CANCEL, OK)
	call ids_grstream (STDIMAGE)

	# read first to clear box
	call gseti(gp, G_CURSOR, IDS_BUT_RD)
	call ggcur(gp, xjunk, yjunk, key)

	i = 1
	repeat {
	    call eprintf("set zoom and zoom center\n")
	    call gseti (gp, G_CURSOR, IDS_BUT_WT)
	    call ggcur(gp, x, y, but)
	    call gseti (gp, G_CURSOR, 1)
	    call ggcur(gp, x, y, key)
	    call zm(gp, but, x, y)
	    call eprintf("set frame, 4 to exit\n")
	    call gseti (gp, G_CURSOR, IDS_BUT_WT)
	    call ggcur(gp, xjunk, yjunk, fnum)
	    if ( fnum == 4)
		break
	    call iset(gp, fnum)
	    repeat {
	        call gseti (gp, G_CURSOR, IDS_BUT_WT)
	        call ggcur(gp, xjunk, yjunk, but)
	        call gseti (gp, G_CURSOR, fnum)
	        call rpc(gp, x, y, key)
		call ggcell (gp, data, 1, 1, x, y, x, y)
		call eprintf("frame %d, datum: %d\n")
		   call pargi (fnum)
		   call pargs (data[1])
	    } until ( but == 4)
	} until ( i == 0 )


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

# zoom

procedure zm(gp, pow, x, y)

int	pow
pointer	gp
real	x, y

short	data[9]

begin
	    data[1] = IDS_ZOOM
	    data[2] = IDS_WRITE
	    data[3] = 3
	    data[4] = IDS_EOD
	    data[5] = IDS_EOD
	    data[6] = 0
	    data[7] = 2**(pow-1)
	    data[8] = x * GKI_MAXNDC
	    data[9] = y * GKI_MAXNDC
	    call gescape ( gp, IDS_CONTROL, data[1], 9)
end

# set image plane for operation

procedure iset (gp, frame)

int	frame
pointer	gp

short	data[10]

begin
	data[1] = frame
	data[2] = IDS_EOD
	data[3] = IDS_EOD	# all bitplanes
	call gescape (gp, IDS_SET_IP, data, 3)
end
