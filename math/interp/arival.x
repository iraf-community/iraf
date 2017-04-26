# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

.help
		arival -- random interpolator returns value

No error traps are included -- unreasonable values for the number of
data points or the x position will either produce hard errors or garbage.
This version is written in-line for speed.

.endhelp

real procedure arival(x, datain, n, interptype)
include "interpdef.h"

real	x		# need 1 <= x <= n
real	datain[ARB]	# data values
int	n		# number of data values
int	interptype

int 	i, k, nx, px
real	a[6], cd20, cd21, cd40, cd41, s, t, h
real	bcoeff[SPLPTS+2], temp[SPLPTS+2], pc[4]

begin
	switch (interptype) {
	case IT_NEAREST :
	    return(datain[int(x + 0.5)])
	case IT_LINEAR :
	    nx = x
	    # protect against x = n case
	    # else it will reference datain[n + 1]
	    if(nx >= n)
		nx = nx - 1
	    return((x - nx) * datain[nx+1] + (nx + 1 - x) * datain[nx])
	case IT_POLY3 :
	    nx = x

	    # The major complication is that near the edge interior polynomial
	    # must somehow be defined.
	    k = 0
	    for(i = nx - 1; i <= nx + 2; i = i + 1){
		k = k + 1
		# project data points into temporary array
		if ( i < 1 )
		    a[k] = 2. * datain[1] - datain[2 - i]
		else if ( i > n )
		    a[k] = 2. * datain[n] - datain[2 * n - i]
		else
		    a[k] = datain[i]
	    }

	    s = x - nx
	    t = 1. - s

	    # second central differences
	    cd20 = 1./6. * (a[3] - 2. * a[2] + a[1])
	    cd21 = 1./6. * (a[4] - 2. * a[3] + a[2])

	    return( s * (a[3] + (s * s - 1.) * cd21) +
		    t * (a[2] + (t * t - 1.) * cd20) )
	case IT_POLY5 :
	    nx = x

	    # The major complication is that near the edge interior polynomial
	    # must somehow be defined.
	    k = 0
	    for(i = nx - 2; i <= nx + 3; i = i + 1){
		k = k + 1
		# project data points into temporary array
		if ( i < 1 )
		    a[k] = 2. * datain[1] - datain[2 - i]
		else if ( i > n )
		    a[k] = 2. * datain[n] - datain[2 * n - i]
		else
		    a[k] = datain[i]
	    }

	    s = x - nx
	    t = 1. - s

	    # second central differences
	    cd20 = 1./6. * (a[4] - 2. * a[3] + a[2])
	    cd21 = 1./6. * (a[5] - 2. * a[4] + a[3])

	    # fourth central differences
	    cd40 = 1./120. * (a[1] - 4. * a[2] + 6. * a[3] - 4. * a[4] + a[5])
	    cd41 = 1./120. * (a[2] - 4. * a[3] + 6. * a[4] - 4. * a[5] + a[6])

	    return( s * (a[4] + (s * s - 1.) * (cd21 + (s * s - 4.) * cd41)) +
		    t * (a[3] + (t * t - 1.) * (cd20 + (t * t - 4.) * cd40)) )
	case IT_SPLINE3 :
	    nx = x

	    h = x - nx
	    k = 0
	    # maximum number of points used is SPLPTS
	    for(i = nx - SPLPTS/2 + 1; i <= nx + SPLPTS/2; i = i + 1){
		if(i < 1 || i > n)
		    ;
		else {
		    k = k + 1
		    if(k == 1)
			px = nx - i + 1
		    bcoeff[k+1] = datain[i]
		}
	    }

	    bcoeff[1] = 0.
	    bcoeff[k+2] = 0.

	    # Use special routine for cardinal splines.
	    call iif_spline(bcoeff, temp, k)

	    px = px + 1

	    pc[1] = bcoeff[px-1] + 4. * bcoeff[px] + bcoeff[px+1]
	    pc[2] = 3. * (bcoeff[px+1] - bcoeff[px-1])
	    pc[3] = 3. * (bcoeff[px-1] - 2. * bcoeff[px] + bcoeff[px+1])
	    pc[4] = -bcoeff[px-1] + 3. * bcoeff[px] - 3. * bcoeff[px+1] +
					bcoeff[px+2]
		    
	    return(pc[1] + h * (pc[2] + h * (pc[3] + h * pc[4])))
	}
end
