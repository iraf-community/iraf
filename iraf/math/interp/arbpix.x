# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

.help
		arbpix -- fix bad data

Takes care of bad pixels by replacing the indef's with interpolated values.

In order to replace bad points, the spline interpolator uses a limited
data array, the maximum total length is given by SPLPTS.  

The process is divided as follows:
	1. Take care of points below first good point and above last good
	   good point.
	2. Load an array with only good points.
	3. Interpolate that array.

.endhelp

procedure arbpix(datain,n,dataout,terptype)
include "interpdef.h"
include "asidef.h"

real datain[ARB]		# data in array
int n				# no. of data points
real dataout[ARB]		# data out array - cannot be same as data in
int terptype			# interpolator type - see interpdef.h

int i, badnc 
int k, ka, kb

real iirbfval()

begin

	# count bad points
	badnc = 0
	do i = 1,n
	    if (IS_INDEFR (datain[i]))
		badnc = badnc + 1

	# return if all bad or all good
	if(badnc == n || badnc == 0)
	    return

	
	# find first good point
	for (ka = 1;  IS_INDEFR (datain[ka]);  ka = ka + 1)
	    ;

	# bad points below first good point are set at first value
	do k = 1,ka-1
	    dataout[k] = datain[ka]
	
	# find last good point
	for (kb = n;  IS_INDEFR (datain[kb]);  kb = kb - 1)
	    ;

	# bad points beyond last good point get set at last value
	do k = n, kb+1, -1
	    dataout[k] = datain[kb]

	# load the other points interpolating the bad points as needed
	do k = ka, kb {
	    if (!IS_INDEFR (datain[k]))		# good point
		dataout[k] = datain[k]

	    else 		# bad point -- generate interpolated value
		dataout[k] = iirbfval(datain[ka],kb-ka+1,k-ka+1,terptype)
	}

end

# This part fills a temporary array with good points that bracket the
# bad point and calls the interpolating routine.

real procedure iirbfval(y, n, k, terptype)

real y[ARB]	# data_in array, y[1] and y[n] guaranteed to be good.
int n		# length of data_in array.
int k		# index of bad point to replace.
int terptype

int  j, jj,  pd, pu, tk, pns
real td[SPLPTS], tx[SPLPTS]	# temporary arrays for interpolation

real iirbint()

begin
	# The following test is done to improve speed.
	# This code will work only if subroutines are implemented by
	#    using static storage - i.e. the old internal values survive.
	# This avoids reloading of temporary arrays if
	#    there are consequetive bad points.

	if (!IS_INDEFR (y[k-1])) {
	    # set number of good points needed on each side of bad point
	    switch (terptype) {
	    case IT_NEAREST :
		pns = 1
	    case IT_LINEAR :
		pns = 1
	    case IT_POLY3 :
		pns = 2
	    case IT_POLY5 :
		pns = 3
	    case IT_SPLINE3 :
		pns = SPLPTS / 2
	    }

	    # search down
	    pd = 0
	    for (j = k-1; j >= 1 && pd < pns; j = j-1)
		if (!IS_INDEFR (y[j]))
		    pd = pd + 1

	    # load temp. arrays for values below our indef.
	    tk = 0
	    for(jj = j + 1; jj < k; jj = jj + 1)
		if (!IS_INDEFR (y[jj])) {
		    tk = tk + 1
		    td[tk] = y[jj]
		    tx[tk] = jj
		}

	     # search and load up from indef.
	     pu = 0
	     for (j = k + 1; j <= n && pu < pns; j = j + 1)
		if (!IS_INDEFR (y[j])) {
		     pu = pu + 1
		     tk = tk + 1
		     td[tk] = y[j]
		     tx[tk] = j
		 }
	 }

	 # return value interpolated from these arrays.
	 return(iirbint(real(k), tx, td, tk, pd, terptype))

end


# This part interpolates the temporary arrays.
# It does not represent a general purpose routine because the
# previous part has determined the proper indices etc. so that
# effort is not duplicated here.

real procedure iirbint (x, tx, td, tk, pd, terptype)

real	x		# point to interpolate
real	tx[ARB]		# xvalues
real	td[ARB]		# data values
int 	tk		# size of data array
int	pd		# index such that tx[pd] < x < tx[pd+1]
int 	terptype

int  i, ks, tpol
real cc[4,SPLPTS]
real h

real iipol_terp()

begin
	switch (terptype) {

	case IT_NEAREST :
	    if (x - tx[1] > tx[2] - x)
		return(td[2])
	     else
		return(td[1])

	case IT_LINEAR :
	    return(td[1] + (x - tx[1]) *
			     (td[2] - td[1]) / (tx[2] - tx[1]))

	case IT_SPLINE3 :
	    do i = 1,tk
		cc[1,i] = td[i]
	    cc[2,1] = 0.
	    cc[2,tk] = 0.

	     # use spline routine from C. de Boor's book
	     # A Practical Guide to Splines
	     call cubspl(tx,cc,tk,2,2)
	     h = x - tx[pd]
	     return(cc[1,pd] + h * (cc[2,pd] + h *
			       (cc[3,pd] + h * cc[4,pd]/3.)/2.))

	default :	# one of the polynomial types
	    # allow lower order if not enough points on one side
	    tpol = tk
	    ks = 1
	    if (tk - pd < pd) {
		tpol = 2 * (tk - pd)
		ks = 2 * pd - tk + 1
	    }
	    if (tk - pd > pd)
		tpol = 2 * pd

	    # finally polynomial interpolate
	    return(iipol_terp(tx[ks], td[ks], tpol, x))
	}

end
