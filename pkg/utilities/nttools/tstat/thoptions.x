include "thistogram.h"		# defines NPAR, etc.

# This file contains th_options and th_update.
#
# Phil Hodge, 18-Mar-1994  Subroutines created.

# th_options -- different options for specifying limits
# There are different ways to specify the limits and bin spacing.  This
# routine computes some parameters given others, based on the following:
#
#    dx = (vhigh - vlow) / nbins
#    dx = (chigh - clow) / (nbins - 1)
#    clow = vlow + dx / 2
#    chigh = vhigh - dx / 2
#
# Note that vlow and vhigh correspond to task parameters lowval and highval.

procedure th_options (nbins, vlow, vhigh, dx, clow, chigh,
		got, find_datamin, find_datamax)

int	nbins		# io: number of bins
double	vlow, vhigh	# io: lower and upper limits
double	dx		# io: bin width
double	clow, chigh	# io: centers of low and high bins
bool	got[NPAR]	# o: flags to specify what we have got
bool	find_datamin	# o: true if we need to find minimum data value
bool	find_datamax	# o: true if we need to find maximum data value
#--
bool	fp_equald()

begin
	# These flags will be reset below if we determine the values.
	got[NBINS] = !IS_INDEFI(nbins)
	got[VLOW]  = !IS_INDEFD(vlow)
	got[VHIGH] = !IS_INDEFD(vhigh)
	got[DX]    = !IS_INDEFD(dx)
	got[CLOW]  = !IS_INDEFD(clow)
	got[CHIGH] = !IS_INDEFD(chigh)

	# Check whether low value is greater than high value.
	if (got[VLOW] && got[VHIGH])
	    if (vlow > vhigh)
		call error (1, "lowval must not be larger than highval")
	if (got[CLOW] && got[CHIGH])
	    if (clow > chigh)
		call error (1, "clow must not be larger than chigh")

	# Further checking.
	if (got[VLOW] && got[CLOW])
	    if (vlow > clow)
		call error (1, "lowval must not be larger than clow")
	if (got[CHIGH] && got[VHIGH])
	    if (chigh > vhigh)
		call error (1, "chigh must not be larger than highval")
	if (got[VLOW] && got[CHIGH])
	    if (vlow > chigh)
		call error (1, "lowval must not be larger than chigh")
	if (got[CLOW] && got[VHIGH])
	    if (clow > vhigh)
		call error (1, "clow must not be larger than highval")

	# Set flags to specify what (if anything) we must get from the data.
	# These may be reset below.
	find_datamin = (!got[VLOW] && !got[CLOW])
	find_datamax = (!got[VHIGH] && !got[CHIGH])

	if (got[DX]) {

	    # Was the lower limit specified by the user?
	    if (got[CLOW]) {
		if (got[VLOW]) {
		    if (!fp_equald (clow, vlow + dx/2.d0))
			call error (1, "values of dx, clow, lowval conflict")
		} else {
		    vlow = clow - dx / 2.d0
		    got[VLOW] = true
		}
	    } else if (got[VLOW]) {
		clow = vlow + dx / 2.d0
		got[CLOW] = true
	    }

	    # Was the upper limit specified?
	    if (got[CHIGH]) {
		if (got[VHIGH]) {
		    if (!fp_equald (chigh, vhigh - dx/2.d0))
			call error (1, "values of dx, chigh, highval conflict")
		} else {
		    vhigh = chigh + dx / 2.d0
		    got[VHIGH] = true
		}
	    } else if (got[VHIGH]) {
		chigh = vhigh - dx / 2.d0
		got[CHIGH] = true
	    }

	    # Was the number of bins specified?
	    if (got[NBINS]) {
		if (got[VLOW] && got[VHIGH]) {
		    if (!fp_equald (vhigh - vlow, dx * nbins))
			call error (1, "specified values for limits conflict")
		} else if (got[VLOW]) {
		    vhigh = vlow + dx * nbins
		    chigh = vhigh - dx / 2.d0
		    got[VHIGH] = true
		    got[CHIGH] = true
		    find_datamax = false
		} else if (got[VHIGH]) {
		    vlow = vhigh - dx * nbins
		    clow = vlow + dx / 2.d0
		    got[VLOW] = true
		    got[CLOW] = true
		    find_datamin = false
		}

	    } else if (got[VLOW] && got[VHIGH]) {
		nbins = nint ((vhigh - vlow) / dx)
		if (nbins * dx < vhigh - vlow)
		    nbins = nbins + 1			# round up
		got[NBINS] = true
	    }

	} else if (got[NBINS]) {		# but we don't have dx

	    if (nbins == 1) {
		if (!got[VLOW] && !got[VHIGH] && (got[CLOW] || got[CHIGH])) {
		    call eprintf (
		"nbins = 1, clow or chigh was specified, but dx was not.\n")
		    call error (1, "must specify dx for this case")
		}
	    }

	    if (got[VLOW] && got[VHIGH]) {

		dx = (vhigh - vlow) / double(nbins)
		got[DX] = true
		if (got[CLOW]) {
		    if (!fp_equald (clow, vlow + dx/2.d0))
			call error (1, "clow conflicts with other parameters")
		} else {
		    clow = vlow + dx / 2.d0
		    got[CLOW] = true
		}
		if (got[CHIGH]) {
		    if (!fp_equald (chigh, vhigh - dx/2.d0))
			call error (1, "chigh conflicts with other parameters")
		} else {
		    chigh = vhigh - dx / 2.d0
		    got[CHIGH] = true
		}

	    } else if (got[CLOW] && got[CHIGH]) {

		if (nbins == 1) {
		    if (!fp_equald (clow, chigh))
			call error (1, "nbins = 1, but clow != chigh")
		} else {
		    dx = (chigh - clow) / (double(nbins) - 1.d0)
		    got[DX] = true
		    if (got[VLOW]) {
			if (!fp_equald (vlow, clow - dx/2.d0))
			    call error (1,
				"lowval conflicts with other parameters")
		    } else {
			vlow = clow - dx / 2.d0
			got[VLOW] = true
		    }
		    if (got[VHIGH]) {
			if (!fp_equald (vhigh, chigh + dx/2.d0))
			    call error (1,
				"highval conflicts with other parameters")
		    } else {
			vhigh = chigh + dx / 2.d0
			got[VHIGH] = true
		    }
		}

	    } else if (got[CLOW] && got[VLOW]) {

		dx = (clow - vlow) * 2.d0
		vhigh = vlow + dx * nbins
		chigh = vhigh - dx / 2.d0
		got[DX] = true
		got[VHIGH] = true
		got[CHIGH] = true
		find_datamax = false

	    } else if (got[CHIGH] && got[VHIGH]) {

		dx = (vhigh - chigh) * 2.d0
		vlow = vhigh - dx * nbins
		clow = vlow + dx / 2.d0
		got[DX] = true
		got[VLOW] = true
		got[CLOW] = true
		find_datamin = false

	    } else if (got[CLOW] && got[VHIGH]) {

		dx = (vhigh - clow) / (double(nbins) - 0.5d0)
		vlow = vhigh - dx * nbins
		chigh = vhigh - dx / 2.d0
		got[DX] = true
		got[VLOW] = true
		got[CHIGH] = true

	    } else if (got[VLOW] && got[CHIGH]) {

		dx = (chigh - vlow) / (double(nbins) - 0.5d0)
		clow = vlow + dx / 2.d0
		vhigh = vlow + dx * nbins
		got[DX] = true
		got[CLOW] = true
		got[VHIGH] = true

	    }

	} else if (got[CLOW] && got[VLOW]) {	# but neither dx nor nbins

	    dx = (clow - vlow) * 2.d0
	    got[DX] = true

	} else if (got[CHIGH] && got[VHIGH]) {	# but neither dx nor nbins

	    dx = (vhigh - chigh) * 2.d0
	    got[DX] = true

	} else {
	    call error (1, "you must specify either nbins or dx (or both)")
	}
end

# th_update -- update the limits
# We now have the minimum and maximum data values from the table,
# so we can fill in the values that the user did not specify.
# We need nbins, vlow, vhigh, and dx.  The values of clow and chigh
# are not modified, even if they are still INDEF; the array of flags
# (got) must not be modified, as these flags are used for different
# tables if there is more than one in the input list.

procedure th_update (vmin, vmax, nbins, vlow, vhigh, dx, clow, chigh,
		got, find_datamin, find_datamax)

double	vmin, vmax	# i: min and max data values for current table
int	nbins		# io: number of bins
double	vlow, vhigh	# io: lower and upper limits
double	dx		# io: bin width
double	clow, chigh	# i: centers of low and high bins
bool	got[NPAR]	# i: flags that specify what parameters we already have
bool	find_datamin	# i: true if we had to find minimum data value
bool	find_datamax	# i: true if we had to find maximum data value
#--
double	delta		# for expanding the range to include endpoints

begin
	if (find_datamin && find_datamax) {

	    # Center the data within the range.  We do have either
	    # dx or nbins or both.
	    if (got[DX] && got[NBINS]) {
		delta = (dx * nbins - (vmax - vmin)) / 2.d0
	    } else if (got[NBINS]) {
		if (nbins == 1)
		    delta = (vmax - vmin) / 100.d0	# 100 is arbitrary
		else
		    delta = (vmax - vmin) / (nbins - 1.d0) / 2.d0
	    } else if (got[DX]) {
		nbins = nint ((vmax - vmin) / dx)
		if (nbins * dx <= vmax - vmin)
		    nbins = nbins + 1			# round up
		delta = (dx * nbins - (vmax - vmin)) / 2.d0
	    }
	    vlow = vmin - delta
	    vhigh = vmax + delta
	    if (!got[DX])
		dx = (vhigh - vlow) / nbins

	} else if (find_datamin) {

	    # We don't have both dx and nbins.  If we did, we would have
	    # calculated vlow from vhigh, dx, and nbins.
	    if (got[DX]) {

		nbins = nint ((vhigh - vmin) / dx)	# vhigh is known
		if (nbins * dx <= vhigh - vmin)
		    nbins = nbins + 1			# round up
		vlow = vhigh - dx * nbins

	    } else if (got[NBINS]) {

		if (got[VHIGH]) {
		    if (nbins == 1)
			delta = (vhigh - vmin) / 100.d0	# 100 is arbitrary
		    else
			delta = (vhigh - vmin) / (nbins - 1.d0) / 2.d0
		    vlow = vmin - delta
		    dx = (vhigh - vlow) / nbins
		} else {			# we have chigh but not vhigh
		    if (nbins == 1) {
			vlow = vmin		# this case doesn't make sense
			vhigh = chigh + (chigh - vmin)
			dx = vhigh - vlow
		    } else {			# set clow = vmin
			dx = (chigh - vmin) / (nbins - 1.d0)
			vlow = vmin - dx / 2.d0
			vhigh = chigh + dx / 2.d0
		    }
		}
	    }

	} else if (find_datamax) {

	    # For this case as well, we don't have both dx and nbins.
	    if (got[DX]) {

		nbins = nint ((vmax - vlow) / dx)	# vlow is known
		if (nbins * dx <= vmax - vlow)
		    nbins = nbins + 1			# round up
		vhigh = vlow + dx * nbins

	    } else if (got[NBINS]) {

		if (got[VLOW]) {
		    if (nbins == 1)
			delta = (vmax - vlow) / 100.d0	# 100 is arbitrary
		    else
			delta = (vmax - vlow) / (nbins - 1.d0) / 2.d0
		    vhigh = vmax + delta
		    dx = (vhigh - vlow) / nbins
		} else {			# we have clow but not vlow
		    if (nbins == 1) {
			vhigh = vmax		# this case doesn't make sense
			vlow = clow - (vmax - clow)
			dx = vhigh - vlow
		    } else {			# set chigh = vmax
			dx = (vmax - clow) / (nbins - 1.d0)
			vlow = vmin - dx / 2.d0
			vhigh = chigh + dx / 2.d0
		    }
		}
	    }
	}
end
