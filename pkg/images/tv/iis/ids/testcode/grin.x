# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <error.h>
include <fio.h>
include <fset.h>
include <gki.h>
include "ids.h"

define	DIM	512
define	MCXSCALE	64
define	MCYSCALE	64

# create a grey scale test image, using frames 1 and 2, and 
# position the cursor in the upper right quadrant.

procedure t_im()

pointer	gp
char	device[SZ_FNAME]

pointer	gopen()
int	open()
int	dd[LEN_GKIDD]

short	i,data[DIM+1]
short	display[6]
short	set_image[3]
real	y, sx, sy
int	key

begin
	call clgstr("device", device, SZ_FNAME)
	call ids_open (device, dd)
	call gki_inline_kernel (STDIMAGE, dd)
	gp = gopen ( device, NEW_FILE, STDIMAGE)

	call fseti (STDIMAGE, F_TYPE, SPOOL_FILE)
	call fseti (STDIMAGE, F_CANCEL, OK)
	call ids_grstream(STDIMAGE)

	data[1] = IDS_R_HARD
	call gescape ( gp, IDS_RESET, data, 1)
	# display all frames off
	display[1] = IDS_OFF
	display[2] = IDS_EOD	# all frames
	display[3] = IDS_EOD	# all colors
	display[4] = IDS_EOD	# all quads
	call gescape ( gp, IDS_DISPLAY_I, display, 6)
	# display frames 1, 2 on -- 1 red, 2 green
	display[1] = IDS_ON
	display[2] = 1
	display[3] = IDS_EOD
	display[4] = IDS_RED
	display[5] = IDS_EOD
	display[6] = IDS_EOD	# all quads
	call gescape ( gp, IDS_DISPLAY_I, display, 6)
	display[1] = IDS_ON
	display[2] = 2
	display[3] = IDS_EOD
	display[4] = IDS_GREEN
	display[5] = IDS_EOD
	display[6] = IDS_EOD	# all quads
	call gescape ( gp, IDS_DISPLAY_I, display, 6)

	# now set up grey scale changing upward in frame 1
	set_image[1] = 1
	set_image[2] = IDS_EOD
	set_image[3] = IDS_EOD	# all planes
	call gescape ( gp, IDS_SET_IP, set_image, 3)
	for ( i = 1; i <= DIM ; i = i + 1 ) {
	    call amovks ( i-1, data, DIM)
	    y = real(i-1)/(DIM-1)
	    call gpcell ( gp, data, DIM, 1, 0., y, 1., y)
	}

	# grey scale changing horizontally in frame 2
	set_image[1] = 2
	call gescape ( gp, IDS_SET_IP, set_image, 3)
	do i = 1, DIM
	    data[i] = i-1
	call gpcell ( gp, data, DIM, 1, 0., 0., 1., 1.)

	# set the cursor
	call gscur ( gp, 0.0, 1.0)

	# read cursor
	call ggcur (gp, sx, sy, key)
	call eprintf("cursor read as : (%f,%f) (%d,%d), key %d\n")
	    call pargr (sx)
	    call pargr (sy)
	    call pargi ( int(sx*32767)/64)
	    call pargi ( int(sy*32767)/64)
	    call pargi (key)

	# all done
	call gclose (gp)
	call ids_close
end
