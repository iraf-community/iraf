# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "imd.h"

define	DIM	512
define	MCXSCALE	64
define	MCYSCALE	64

# create a grey scale test image, using frames 1 and 2, and 
# position the cursor in the upper right quadrant.

procedure t_im()

pointer	gp
char	output[SZ_FNAME], output_file[SZ_FNAME], device[SZ_FNAME]
int	fd

pointer	gopen()
bool	streq()
int	open()

short	i,data[DIM+1]
short	display[6]
short	set_image[3]
real	y, sx, sy
int	key

begin
	call clgstr("output", output, SZ_FNAME)
	if (!streq (output, "") ) {
	    call strcpy (output, output_file, SZ_FNAME)
	    fd = open (output_file, NEW_FILE, BINARY_FILE)
	} else
	    fd = open ("dev$stdimage", NEW_FILE, BINARY_FILE)

	call clgstr("device", device, SZ_FNAME)
	gp = gopen ( device, NEW_FILE, fd)

	data[1] = IMD_R_HARD
	call gescape ( gp, IMD_RESET, data, 1)
	# display all frames off
	display[1] = IMD_OFF
	display[2] = IMD_EOD	# all frames
	display[3] = IMD_EOD	# all colors
	display[4] = IMD_EOD	# all quads
	call gescape ( gp, IMD_DISPLAY_I, display, 6)
	# display frames 1, 2 on -- 1 red, 2 green
	display[1] = IMD_ON
	display[2] = 1
	display[3] = IMD_EOD
	display[4] = IMD_RED
	display[5] = IMD_EOD
	display[6] = IMD_EOD	# all quads
	call gescape ( gp, IMD_DISPLAY_I, display, 6)
	display[1] = IMD_ON
	display[2] = 2
	display[3] = IMD_EOD
	display[4] = IMD_GREEN
	display[5] = IMD_EOD
	display[6] = IMD_EOD	# all quads
	call gescape ( gp, IMD_DISPLAY_I, display, 6)

	# now set up grey scale changing upward in frame 1
	set_image[1] = 1
	set_image[2] = IMD_EOD
	set_image[3] = IMD_EOD	# all planes
	call gescape ( gp, IMD_SET_IP, set_image, 3)
	for ( i = 1; i <= DIM ; i = i + 1 ) {
	    call amovks ( i-1, data, DIM)
	    y = real(i-1)/(DIM-1)
	    call gpcell ( gp, data, DIM, 1, 0., y, 1., y)
	}

	# grey scale changing horizontally in frame 2
	set_image[1] = 2
	call gescape ( gp, IMD_SET_IP, set_image, 3)
	do i = 1, DIM
	    data[i] = i
	call gpcell ( gp, data, DIM, 1, 0., 0., 1., 1.)

	# set the cursor
	call gscur ( gp, 0.0, 1.0)

	# read cursor
	# call ggcur( gp, sx, sy, key)

	# all done
	call gclose ( gp )
	call close ( fd )
end
