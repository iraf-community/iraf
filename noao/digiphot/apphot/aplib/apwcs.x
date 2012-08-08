# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <imio.h>
include "../lib/apphot.h"

# AP_ITOL -- Convert coordinates from the input coordinate system to the
# logical coordinate system.

procedure ap_itol (ap, xin, yin, xout, yout, npts)

pointer	ap			# the apphot package descriptor
real	xin[ARB]		# the input x coordinate
real	yin[ARB]		# the input y coordinate
real	xout[ARB]		# the output x coordinate 
real	yout[ARB]		# the output y coordinate
int	npts			# the number of coordinates to convert

double	xt, yt
pointer	ct
int	i
int	apstati()

begin
	ct = apstati (ap, CTIN)
	if (ct == NULL) {
	    call amovr (xin, xout, npts)
	    call amovr (yin, yout, npts)
	    return
	}

	do i = 1, npts {
	    call mw_c2trand (ct, double (xin[i]), double (yin[i]), xt, yt) 
	    xout[i] = xt
	    yout[i] = yt
	}
end


# AP_LTOO -- Convert coordinates from the logical coordinate system to the
# output coordinate system.

procedure ap_ltoo (ap, xin, yin, xout, yout, npts)

pointer	ap			# the apphot package descriptor
real	xin[ARB]		# the input x coordinate
real	yin[ARB]		# the input y coordinate
real	xout[ARB]		# the output x coordinate 
real	yout[ARB]		# the output y coordinate
int	npts			# the number of coordinates to convert

double	xt, yt
pointer	ct
int	i
int	apstati()

begin
	ct = apstati (ap, CTOUT)
	if (ct == NULL) {
	    call amovr (xin, xout, npts)
	    call amovr (yin, yout, npts)
	    return
	}

	do i = 1, npts {
	    call mw_c2trand (ct, double (xin[i]), double (yin[i]), xt, yt) 
	    xout[i] = xt
	    yout[i] = yt
	}
end


# AP_LTOV -- Convert coordinate from the logical coordinate system to the
# output coordinate system.

procedure ap_ltov (im, xin, yin, xout, yout, npts)

pointer	im			# the input image descriptor
real	xin[ARB]		# the input x coordinate
real	yin[ARB]		# the input y coordinate
real	xout[ARB]		# the output x coordinate 
real	yout[ARB]		# the output y coordinate
int	npts			# the number of coordinates to convert

int	i, index1, index2

begin
	index1 = IM_VMAP(im,1)
	index2 = IM_VMAP(im,2)
	do i = 1, npts {
	    xout[i] = xin[i] * IM_VSTEP(im,index1) + IM_VOFF(im,index1)
	    yout[i] = yin[i] * IM_VSTEP(im,index2) + IM_VOFF(im,index2)
	}
end


# AP_VTOL -- Convert coordinate from the tv coordinate system to the
# logical coordinate system.

procedure ap_vtol (im, xin, yin, xout, yout, npts)

pointer	im			# the input image descriptor
real	xin[ARB]		# the input x coordinate
real	yin[ARB]		# the input y coordinate
real	xout[ARB]		# the output x coordinate 
real	yout[ARB]		# the output y coordinate
int	npts			# the number of coordinates to convert

int	i, index1, index2

begin
	index1 = IM_VMAP(im,1)
	index2 = IM_VMAP(im,2)
	do i = 1, npts {
	    xout[i] = (xin[i] - IM_VOFF(im,index1)) / IM_VSTEP(im,index1)
	    yout[i] = (yin[i] - IM_VOFF(im,index2)) / IM_VSTEP(im,index2)
	}
end
