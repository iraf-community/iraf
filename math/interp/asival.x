# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

.help
		procedure asival

     This procedure finds interpolated value assuming that x lands in
array i.e. 1 <= x < npts. 
It must be called by a routine that checks for out of bound references
and takes care of bad pixels.
     This version assumes that the data are stored for polynomials and
the spline appears as basis-spline coefficients.
     Also the sequential evaluators are used to obtain the values in
order to reduce the amount of duplicated code.
.endhelp

real procedure asival(x,coeff)
include "interpdef.h"
include "asidef.h"

real x
real coeff[ARB]

real t

begin
	switch (ITYPEI)	{	# switch on interpolator type
	
	case IT_NEAREST :
	    call iievne(x,t,1,coeff[COFF+1])
	    return(t)

	case IT_LINEAR :
	    call iievli(x,t,1,coeff[COFF+1])
	    return(t)

	case IT_POLY3 :
	    call iievp3(x,t,1,coeff[COFF+1])
	    return(t)

	case IT_POLY5 :
	    call iievp5(x,t,1,coeff[COFF+1])
	    return(t)

	case IT_SPLINE3 :
	    call iievs3(x,t,1,coeff[COFF+1])
	    return(t)

	}
end
