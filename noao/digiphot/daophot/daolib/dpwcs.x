# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <imio.h>
include "../lib/daophotdef.h"

# DP_WIN -- Convert the input coordinates to logical coordinates.

procedure dp_win (dp, im, xin, yin, xout, yout, npts)

pointer	dp			# the apphot package descriptor
pointer	im			# the input image descriptor
real	xin[ARB]		# the input x coordinate
real	yin[ARB]		# the input y coordinate
real	xout[ARB]		# the output x coordinate 
real	yout[ARB]		# the output y coordinate
int	npts			# the number of coordinates to convert

int	dp_stati()

begin
        # Transform the input coordinates.
        switch (dp_stati (dp, WCSIN)) {
        case WCS_WORLD, WCS_PHYSICAL:
            call dp_itol (dp, xin, yin, xout, yout, npts)
        case WCS_TV:
            call dp_vtol (im, xin, yin, xout, yout, npts)
        default:
	    call amovr (xin, xout, npts)
	    call amovr (yin, yout, npts)
        }
end


# DP_WOUT -- Convert the logical coordinates to output coordinates.

procedure dp_wout (dp, im, xin, yin, xout, yout, npts)

pointer	dp			# the apphot package descriptor
pointer	im			# the input image descriptor
real	xin[ARB]		# the input x coordinate
real	yin[ARB]		# the input y coordinate
real	xout[ARB]		# the output x coordinate 
real	yout[ARB]		# the output y coordinate
int	npts			# the number of coordinates to convert

int	dp_stati()

begin
        # Transform the input coordinates.
        switch (dp_stati (dp, WCSOUT)) {
        case WCS_WORLD, WCS_PHYSICAL:
            call dp_ltoo (dp, xin, yin, xout, yout, npts)
        case WCS_TV:
            call dp_ltov (im, xin, yin, xout, yout, npts)
        default:
	    call amovr (xin, xout, npts)
	    call amovr (yin, yout, npts)
        }
end


# DP_WPSF -- Convert the logical coordinates to psf coordinates.

procedure dp_wpsf (dp, im, xin, yin, xout, yout, npts)

pointer	dp			# the apphot package descriptor
pointer	im			# the input image descriptor
real	xin[ARB]		# the input x coordinate
real	yin[ARB]		# the input y coordinate
real	xout[ARB]		# the output x coordinate 
real	yout[ARB]		# the output y coordinate
int	npts			# the number of coordinates to convert

int	dp_stati()

begin
        # Transform the input coordinates.
        switch (dp_stati (dp, WCSPSF)) {
        case WCS_WORLD, WCS_PHYSICAL:
            call dp_ltop (dp, xin, yin, xout, yout, npts)
        case WCS_TV:
            call dp_ltov (im, xin, yin, xout, yout, npts)
        default:
	    call amovr (xin, xout, npts)
	    call amovr (yin, yout, npts)
        }
end


# DP_ITOL -- Convert coordinates from the input coordinate system to the
# logical coordinate system.

procedure dp_itol (dp, xin, yin, xout, yout, npts)

pointer	dp			# the apphot package descriptor
real	xin[ARB]		# the input x coordinate
real	yin[ARB]		# the input y coordinate
real	xout[ARB]		# the output x coordinate 
real	yout[ARB]		# the output y coordinate
int	npts			# the number of coordinates to convert

double	xt, yt
pointer	ct
int	i
int	dp_stati()

begin
	ct = dp_stati (dp, CTIN)
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


# DP_LTOO -- Convert coordinates from the logical coordinate system to the
# output coordinate system.

procedure dp_ltoo (dp, xin, yin, xout, yout, npts)

pointer	dp			# the apphot package descriptor
real	xin[ARB]		# the input x coordinate
real	yin[ARB]		# the input y coordinate
real	xout[ARB]		# the output x coordinate 
real	yout[ARB]		# the output y coordinate
int	npts			# the number of coordinates to convert

double	xt, yt
pointer	ct
int	i
int	dp_stati()

begin
	ct = dp_stati (dp, CTOUT)
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


# DP_LTOP -- Convert coordinates from the logical coordinate system to the
# output coordinate system.

procedure dp_ltop (dp, xin, yin, xout, yout, npts)

pointer	dp			# the apphot package descriptor
real	xin[ARB]		# the input x coordinate
real	yin[ARB]		# the input y coordinate
real	xout[ARB]		# the output x coordinate 
real	yout[ARB]		# the output y coordinate
int	npts			# the number of coordinates to convert

double	xt, yt
pointer	ct
int	i
int	dp_stati()

begin
	ct = dp_stati (dp, CTPSF)
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


# DP_LTOV -- Convert coordinate from the logical coordinate system to the
# output coordinate system.

procedure dp_ltov (im, xin, yin, xout, yout, npts)

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


# DP_VTOL -- Convert coordinate from the tv coordinate system to the
# logical coordinate system.

procedure dp_vtol (im, xin, yin, xout, yout, npts)

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
