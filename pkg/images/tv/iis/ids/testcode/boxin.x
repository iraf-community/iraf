# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <error.h>
include <fio.h>
include <fset.h>
include "ids.h"
include <gki.h>

define	DIM	512
define	MCXSCALE	64
define	MCYSCALE	64

# create a box test image

procedure t_im()

pointer	gp
char	device[SZ_FNAME]

pointer	gopen()
int	dd[LEN_GKIDD]

short	i,data[DIM+1]
short	set_image[6]
int	key, j
real	x[30],y[30]
real	lb,ub,mid
int	mod()

begin
	call clgstr("device", device, SZ_FNAME)
	call ids_open (device, dd)
	call gki_inline_kernel (STDIMAGE, dd)
	gp = gopen ( device, NEW_FILE, STDIMAGE)

	call fseti (STDIMAGE, F_TYPE, SPOOL_FILE)
	call fseti (STDIMAGE, F_CANCEL, OK)

	# enable the blue plane
	set_image[1] = IDS_ON
	set_image[2] = IDS_EOD		# all graphics frames
	set_image[3] = IDS_BLUE		# color
	set_image[4] = IDS_EOD
	set_image[5] = IDS_EOD		# all quadrants
	call gescape ( gp, IDS_DISPLAY_G, set_image, 5)

	# set which plane to write into
	set_image[1] = 1
	set_image[2] = IDS_EOD		# first graphics frame
	set_image[3] = IDS_BLUE		# color
	set_image[4] = IDS_EOD
	call gescape ( gp, IDS_SET_GP, set_image, 4)

	# now set up boxes
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
		y[2] = y[1] + mid - lb
		x[3] = y[2]
		y[3] = y[2]
		x[4] = y[2]
		y[4] = x[1]
		x[5] = x[1]
		y[5] = y[1]
		lb = x[1]
		ub = y[2]
	    }
	    do j = 1,5 {
		x[j] = x[j] * 32768. / 32767.
		if (x[j] > 1.0)
		    x[j] = 1.0
		y[j] = y[j] * 32768. / 32767.
		if (y[j] > 1.0)
		    y[j] = 1.0
	    }
	    call gpline ( gp, x, y, 5)
	}

	# all done
	call gclose ( gp )
	call ids_close
end
