# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "imd.h"
include <gki.h>

define	DIM	512
define	MCXSCALE	64
define	MCYSCALE	64

# create a box test image

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
real	lb,ub,mid
int	mod()

begin
	call clgstr("output", output, SZ_FNAME)
	if (!streq (output, "") ) {
	    call strcpy (output, output_file, SZ_FNAME)
	    fd = open (output_file, NEW_FILE, BINARY_FILE)
	} else
	    fd = open ("dev$stdimage", NEW_FILE, BINARY_FILE)

	call clgstr("device", device, SZ_FNAME)
	gp = gopen ( device, NEW_FILE, fd)

	# now set up boxes
	set_image[1] = 1
	set_image[2] = IMD_EOD
	set_image[3] = IMD_BLUE
	set_image[4] = IMD_EOD
	call gescape ( gp, IMD_SET_GP, set_image, 4)
        lb = 0.0
        ub = 1.0
	mid = (lb + ub)/2.
	for ( i = 1; i <= 5 ; i = i + 1 ) {
	    if ( mod(i-1,2) == 0 ) {
		x[1] = lb
		y[1] = mid
		x[2] = mid
		y[2] = ub
		x[3] = ub
		y[3] = mid
		x[4] = mid
		y[4] = lb
		x[5] = lb
		y[5] = mid
	    } else {
		x[1] = (mid-lb)/2 + lb
		y[1] = x[1]
		x[2] = x[1]
		# x[2] = x[1] - .05
		y[2] = y[1] + mid - lb
		x[3] = y[2]
		y[3] = y[2]
		# y[3] = y[2] - .05
		x[4] = y[2]
		y[4] = x[1]
		x[5] = x[1]
		y[5] = y[1]
		lb = x[1]
		ub = y[2]
	    }
	    call gpline ( gp, x, y, 5)
	}

	# all done
	call gclose ( gp )
	call close ( fd )
end
