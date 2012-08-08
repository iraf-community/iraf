# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

.help
	arider -- random interpolator returns derivatives

First looks at type of interpolator requested, then calls routine
to evaluate.
No error traps are included -- unreasonable values for the number of
data points or the x position will either produce hard errors or garbage.

.endhelp

procedure arider(x, datain, n, derivs, nderiv,  interptype)
include "interpdef.h"

real	x		# need 1 <= x <= n
real	datain[ARB]	# data values
int	n		# number of data values
real	derivs[ARB]	# derivatives out -- beware derivs[1] is 
			#		function value
int	nderiv		# total number of values returned in derivs
int	interptype

int	i, j, k, nt, nd, nx
real	pc[6], ac, s

begin
	if(nderiv <= 0)
	    return

	# zero out derivs array
	do i = 1, nderiv
	    derivs[i] = 0.

	switch (interptype) {
	case IT_NEAREST :
	    derivs[1] = datain[int(x + 0.5)]
	    return
	case IT_LINEAR :
	    nx = x
	    if( nx >= n )
		nx = nx - 1
	    derivs[1] = (x - nx) * datain[nx+1] + (nx + 1 - x) * datain[nx]
	    if(nderiv >= 2)
		derivs[2] = datain[nx+1] - datain[nx]
	    return
	# The other cases call subroutines to generate polynomial coeff.
	case IT_POLY3 :
	    call iidr_poly3(x, datain, n, pc)
	    nt = 4
	case IT_POLY5 :
	    call iidr_poly5(x, datain, n, pc)
	    nt = 6
	case IT_SPLINE3 :
	    call iidr_spline3(x, datain, n, pc)
	    nt = 4
	}

	nx = x
	s = x - nx
	nd = nderiv
	if (nderiv > nt)
	    nd = nt

	do k = 1,nd {	
	    ac = pc[nt - k + 1]		# evluate using nested multiplication
	    do j = nt - k, 1, -1
		ac = pc[j] + s * ac

	    derivs[k] = ac

	    do j = 1, nt - k		# differentiate
		pc[j] = j * pc[j + 1]
	}
end

procedure iidr_poly3(x, datain, n, pc)

real	x
real	datain[ARB]
int	n
real	pc[ARB]

int 	i, k, nx, nt
real	a[4]

begin

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

	nt = 4

	# generate diffrence table for Newton's form
	do k = 1, nt-1
	    do i = 1, nt-k
		a[i] = (a[i+1] - a[i]) / k
	
	# shift to generate polynomial coefficients
	do k = nt,2,-1
	    do i = 2,k
		a[i] = a[i] + a[i-1] * (k - i - nt/2)
	
	do i = 1,nt
	    pc[i] = a[nt+1-i]
	
	return

end

procedure iidr_poly5(x, datain, n, pc)

real	x
real	datain[ARB]
int	n
real	pc[ARB]

int 	i, k, nx, nt
real	a[6]

begin

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

	nt = 6

	# generate diffrence table for Newton's form
	do k = 1, nt-1
	    do i = 1, nt-k
		a[i] = (a[i+1] - a[i]) / k
	
	# shift to generate polynomial coefficients
	do k = nt,2,-1
	    do i = 2,k
		a[i] = a[i] + a[i-1] * (k - i - nt/2)
	
	do i = 1,nt
	    pc[i] = a[nt+1-i]
	
	return

end

procedure iidr_spline3(x, datain, n, pc)

real	x
real	datain[ARB]
int	n
real	pc[ARB]

int 	i, k, nx, px
real	temp[SPLPTS+2], bcoeff[SPLPTS+2],  h

begin

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
		
	return
end
