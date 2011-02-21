include <imhdr.h>
include "pvol.h"



# VTRANSMIT -- Compute the intensities of each output image pixel in the
# current line as a function of its existing intensity plus the emission
# and absorption from each contributing voxel.

procedure vtransmits (inbuf,nx,ny,nz,nh, px1,px2, iline,iband,nvox, oline, vp)
short	inbuf[nx,ny,nz,nh]	# Input data buffer for current set of yz slices
int	nx,ny,nz,nh		# Dimensions of current input buffer
int	px1,px2			# Range of columns in current yz slice set
int	iline[nvox]		# Input image lines for current projection ray
int	iband[nvox]		# Input image bands for current projection ray
int	nvox			# Number of voxels in current projection column
real	oline[ARB]		# output image line buffer
pointer	vp			# Volume projection descriptor

bool	use_both

int	i, vox, opelem, intelem, frontvox, backvox
real	amin, amax, vox_op, vox_int, ival, attenuate, vimin, ifac, ofac, distwt

begin
	# Dereference most frequently used structure elements.
	amin = AMIN(vp)
	amax = AMAX(vp)
	vimin = VIMIN(vp)

	intelem = 1
	opelem = OPACELEM(vp)
	if (nh > 1) {
	    use_both = true
	    if (opelem == 1)
		intelem = 2
	    else if (IS_INDEFI(opelem))
		opelem = 2
	} else {
	    use_both = false
	    opelem = 1
	}


	# Set up for opacity, intensity, or both.
	ifac = (IIMAX(vp) - IIMIN(vp)) / (VIMAX(vp) - vimin)
	if (PTYPE(vp) == P_ATTENUATE || use_both)
	    ofac = (amax - amin) / (OMAX(vp) - OMIN(vp))

	# Since we are in memory anyway, it is more convenient to traverse
	# the columns in the outer loop and the voxels from different bands
	# and lines in the inner loop.  This is necessary when distance
	# weighting and the distance cutoff option is on (we need to know
	# the range of usable voxels in a given column before projecting).

	if (PTYPE(vp) == P_INVDISPOW || PTYPE(vp) == P_MODN)
	    do i = px1, px2 {
		if (DISCUTOFF(vp) == NO) {
		    frontvox = nvox
		    backvox = 1
		} else {
		    frontvox = 1
		    backvox = nvox
		    do vox = 1, nvox {
			vox_int = inbuf[(i-px1+1),iline[vox],iband[vox],intelem]
			if (vimin <= vox_int && vox_int < VIMAX(vp)) {
			    frontvox = max (frontvox, vox)
			    backvox = min (backvox, vox)
			}
		    }
		}
		if (frontvox - backvox < 0)
		    next
		do vox = backvox, frontvox {
		    distwt = (real(vox-backvox+1) /
			real(frontvox-backvox+1)) ** DISPOWER(vp)

		    # Opacity transformation function.
		    if (use_both) {
			vox_op = inbuf[(i-px1+1), iline[vox], iband[vox],
			    opelem] * OSCALE(vp)
			if (vox_op < OMIN(vp))
			    attenuate = amax
			else if (OMIN(vp) <= vox_op && vox_op < OMAX(vp))
			    attenuate = amax - (vox_op - OMIN(vp)) * ofac
			else
			    attenuate = amin
			oline[i] = oline[i] * attenuate
		    }

		    # Intensity transformation function.
		    vox_int = inbuf[(i-px1+1),iline[vox],iband[vox],
			intelem]
		    if (vox_int < vimin)
			ival = IIMIN(vp)
		    else if (vimin <= vox_int && vox_int < VIMAX(vp))
			ival = IIMIN(vp) + (vox_int - vimin) * ifac
		    else
			ival = IIMAX(vp)

		    if (PTYPE(vp) == P_INVDISPOW)
			oline[i] = oline[i] + ival * distwt
		    else if (mod (int (ival/(IIMAX(vp)-IIMIN(vp)) * 100.0),
			    MODN(vp)) == 0)
			    oline[i] = oline[i] + ival * distwt
		}
	    }
	else
	    do i = px1, px2
		do vox = 1, nvox {
		    # Opacity transformation function.
		    if (PTYPE(vp) == P_ATTENUATE || use_both) {
			vox_op = inbuf[(i-px1+1), iline[vox], iband[vox],
			    opelem] * OSCALE(vp)
			if (vox_op < OMIN(vp))
			    attenuate = amax
			else if (OMIN(vp) <= vox_op && vox_op < OMAX(vp))
			    attenuate = amax - (vox_op - OMIN(vp)) * ofac
			else
			    attenuate = amin
			oline[i] = oline[i] * attenuate
		    }

		    # Intensity transformation function.
		    if (PTYPE(vp) != P_ATTENUATE || use_both) {
			vox_int = inbuf[(i-px1+1),iline[vox],iband[vox],
			    intelem]
			if (vox_int < vimin)
			    ival = IIMIN(vp)
			else if (vimin <= vox_int && vox_int < VIMAX(vp))
			    ival = IIMIN(vp) + (vox_int - vimin) * ifac
			else
			    ival = IIMAX(vp)

			if (PTYPE(vp) == P_AVERAGE)
			    oline[i] = oline[i] + ival * 1.0 / real(nvox)
			else if (PTYPE(vp) == P_SUM)
			    oline[i] = oline[i] + ival
			else if (PTYPE(vp) == P_LASTONLY)
			    if (ival > 0.0)
				oline[i] = ival
		    }
		}
end



# VTRANSMIT -- Compute the intensities of each output image pixel in the
# current line as a function of its existing intensity plus the emission
# and absorption from each contributing voxel.

procedure vtransmiti (inbuf,nx,ny,nz,nh, px1,px2, iline,iband,nvox, oline, vp)
int	inbuf[nx,ny,nz,nh]	# Input data buffer for current set of yz slices
int	nx,ny,nz,nh		# Dimensions of current input buffer
int	px1,px2			# Range of columns in current yz slice set
int	iline[nvox]		# Input image lines for current projection ray
int	iband[nvox]		# Input image bands for current projection ray
int	nvox			# Number of voxels in current projection column
real	oline[ARB]		# output image line buffer
pointer	vp			# Volume projection descriptor

bool	use_both

int	i, vox, opelem, intelem, frontvox, backvox
real	amin, amax, vox_op, vox_int, ival, attenuate, vimin, ifac, ofac, distwt

begin
	# Dereference most frequently used structure elements.
	amin = AMIN(vp)
	amax = AMAX(vp)
	vimin = VIMIN(vp)

	intelem = 1
	opelem = OPACELEM(vp)
	if (nh > 1) {
	    use_both = true
	    if (opelem == 1)
		intelem = 2
	    else if (IS_INDEFI(opelem))
		opelem = 2
	} else {
	    use_both = false
	    opelem = 1
	}


	# Set up for opacity, intensity, or both.
	ifac = (IIMAX(vp) - IIMIN(vp)) / (VIMAX(vp) - vimin)
	if (PTYPE(vp) == P_ATTENUATE || use_both)
	    ofac = (amax - amin) / (OMAX(vp) - OMIN(vp))

	# Since we are in memory anyway, it is more convenient to traverse
	# the columns in the outer loop and the voxels from different bands
	# and lines in the inner loop.  This is necessary when distance
	# weighting and the distance cutoff option is on (we need to know
	# the range of usable voxels in a given column before projecting).

	if (PTYPE(vp) == P_INVDISPOW || PTYPE(vp) == P_MODN)
	    do i = px1, px2 {
		if (DISCUTOFF(vp) == NO) {
		    frontvox = nvox
		    backvox = 1
		} else {
		    frontvox = 1
		    backvox = nvox
		    do vox = 1, nvox {
			vox_int = inbuf[(i-px1+1),iline[vox],iband[vox],intelem]
			if (vimin <= vox_int && vox_int < VIMAX(vp)) {
			    frontvox = max (frontvox, vox)
			    backvox = min (backvox, vox)
			}
		    }
		}
		if (frontvox - backvox < 0)
		    next
		do vox = backvox, frontvox {
		    distwt = (real(vox-backvox+1) /
			real(frontvox-backvox+1)) ** DISPOWER(vp)

		    # Opacity transformation function.
		    if (use_both) {
			vox_op = inbuf[(i-px1+1), iline[vox], iband[vox],
			    opelem] * OSCALE(vp)
			if (vox_op < OMIN(vp))
			    attenuate = amax
			else if (OMIN(vp) <= vox_op && vox_op < OMAX(vp))
			    attenuate = amax - (vox_op - OMIN(vp)) * ofac
			else
			    attenuate = amin
			oline[i] = oline[i] * attenuate
		    }

		    # Intensity transformation function.
		    vox_int = inbuf[(i-px1+1),iline[vox],iband[vox],
			intelem]
		    if (vox_int < vimin)
			ival = IIMIN(vp)
		    else if (vimin <= vox_int && vox_int < VIMAX(vp))
			ival = IIMIN(vp) + (vox_int - vimin) * ifac
		    else
			ival = IIMAX(vp)

		    if (PTYPE(vp) == P_INVDISPOW)
			oline[i] = oline[i] + ival * distwt
		    else if (mod (int (ival/(IIMAX(vp)-IIMIN(vp)) * 100.0),
			    MODN(vp)) == 0)
			    oline[i] = oline[i] + ival * distwt
		}
	    }
	else
	    do i = px1, px2
		do vox = 1, nvox {
		    # Opacity transformation function.
		    if (PTYPE(vp) == P_ATTENUATE || use_both) {
			vox_op = inbuf[(i-px1+1), iline[vox], iband[vox],
			    opelem] * OSCALE(vp)
			if (vox_op < OMIN(vp))
			    attenuate = amax
			else if (OMIN(vp) <= vox_op && vox_op < OMAX(vp))
			    attenuate = amax - (vox_op - OMIN(vp)) * ofac
			else
			    attenuate = amin
			oline[i] = oline[i] * attenuate
		    }

		    # Intensity transformation function.
		    if (PTYPE(vp) != P_ATTENUATE || use_both) {
			vox_int = inbuf[(i-px1+1),iline[vox],iband[vox],
			    intelem]
			if (vox_int < vimin)
			    ival = IIMIN(vp)
			else if (vimin <= vox_int && vox_int < VIMAX(vp))
			    ival = IIMIN(vp) + (vox_int - vimin) * ifac
			else
			    ival = IIMAX(vp)

			if (PTYPE(vp) == P_AVERAGE)
			    oline[i] = oline[i] + ival * 1.0 / real(nvox)
			else if (PTYPE(vp) == P_SUM)
			    oline[i] = oline[i] + ival
			else if (PTYPE(vp) == P_LASTONLY)
			    if (ival > 0.0)
				oline[i] = ival
		    }
		}
end



# VTRANSMIT -- Compute the intensities of each output image pixel in the
# current line as a function of its existing intensity plus the emission
# and absorption from each contributing voxel.

procedure vtransmitl (inbuf,nx,ny,nz,nh, px1,px2, iline,iband,nvox, oline, vp)
long	inbuf[nx,ny,nz,nh]	# Input data buffer for current set of yz slices
int	nx,ny,nz,nh		# Dimensions of current input buffer
int	px1,px2			# Range of columns in current yz slice set
int	iline[nvox]		# Input image lines for current projection ray
int	iband[nvox]		# Input image bands for current projection ray
int	nvox			# Number of voxels in current projection column
real	oline[ARB]		# output image line buffer
pointer	vp			# Volume projection descriptor

bool	use_both

int	i, vox, opelem, intelem, frontvox, backvox
real	amin, amax, vox_op, vox_int, ival, attenuate, vimin, ifac, ofac, distwt

begin
	# Dereference most frequently used structure elements.
	amin = AMIN(vp)
	amax = AMAX(vp)
	vimin = VIMIN(vp)

	intelem = 1
	opelem = OPACELEM(vp)
	if (nh > 1) {
	    use_both = true
	    if (opelem == 1)
		intelem = 2
	    else if (IS_INDEFI(opelem))
		opelem = 2
	} else {
	    use_both = false
	    opelem = 1
	}


	# Set up for opacity, intensity, or both.
	ifac = (IIMAX(vp) - IIMIN(vp)) / (VIMAX(vp) - vimin)
	if (PTYPE(vp) == P_ATTENUATE || use_both)
	    ofac = (amax - amin) / (OMAX(vp) - OMIN(vp))

	# Since we are in memory anyway, it is more convenient to traverse
	# the columns in the outer loop and the voxels from different bands
	# and lines in the inner loop.  This is necessary when distance
	# weighting and the distance cutoff option is on (we need to know
	# the range of usable voxels in a given column before projecting).

	if (PTYPE(vp) == P_INVDISPOW || PTYPE(vp) == P_MODN)
	    do i = px1, px2 {
		if (DISCUTOFF(vp) == NO) {
		    frontvox = nvox
		    backvox = 1
		} else {
		    frontvox = 1
		    backvox = nvox
		    do vox = 1, nvox {
			vox_int = inbuf[(i-px1+1),iline[vox],iband[vox],intelem]
			if (vimin <= vox_int && vox_int < VIMAX(vp)) {
			    frontvox = max (frontvox, vox)
			    backvox = min (backvox, vox)
			}
		    }
		}
		if (frontvox - backvox < 0)
		    next
		do vox = backvox, frontvox {
		    distwt = (real(vox-backvox+1) /
			real(frontvox-backvox+1)) ** DISPOWER(vp)

		    # Opacity transformation function.
		    if (use_both) {
			vox_op = inbuf[(i-px1+1), iline[vox], iband[vox],
			    opelem] * OSCALE(vp)
			if (vox_op < OMIN(vp))
			    attenuate = amax
			else if (OMIN(vp) <= vox_op && vox_op < OMAX(vp))
			    attenuate = amax - (vox_op - OMIN(vp)) * ofac
			else
			    attenuate = amin
			oline[i] = oline[i] * attenuate
		    }

		    # Intensity transformation function.
		    vox_int = inbuf[(i-px1+1),iline[vox],iband[vox],
			intelem]
		    if (vox_int < vimin)
			ival = IIMIN(vp)
		    else if (vimin <= vox_int && vox_int < VIMAX(vp))
			ival = IIMIN(vp) + (vox_int - vimin) * ifac
		    else
			ival = IIMAX(vp)

		    if (PTYPE(vp) == P_INVDISPOW)
			oline[i] = oline[i] + ival * distwt
		    else if (mod (int (ival/(IIMAX(vp)-IIMIN(vp)) * 100.0),
			    MODN(vp)) == 0)
			    oline[i] = oline[i] + ival * distwt
		}
	    }
	else
	    do i = px1, px2
		do vox = 1, nvox {
		    # Opacity transformation function.
		    if (PTYPE(vp) == P_ATTENUATE || use_both) {
			vox_op = inbuf[(i-px1+1), iline[vox], iband[vox],
			    opelem] * OSCALE(vp)
			if (vox_op < OMIN(vp))
			    attenuate = amax
			else if (OMIN(vp) <= vox_op && vox_op < OMAX(vp))
			    attenuate = amax - (vox_op - OMIN(vp)) * ofac
			else
			    attenuate = amin
			oline[i] = oline[i] * attenuate
		    }

		    # Intensity transformation function.
		    if (PTYPE(vp) != P_ATTENUATE || use_both) {
			vox_int = inbuf[(i-px1+1),iline[vox],iband[vox],
			    intelem]
			if (vox_int < vimin)
			    ival = IIMIN(vp)
			else if (vimin <= vox_int && vox_int < VIMAX(vp))
			    ival = IIMIN(vp) + (vox_int - vimin) * ifac
			else
			    ival = IIMAX(vp)

			if (PTYPE(vp) == P_AVERAGE)
			    oline[i] = oline[i] + ival * 1.0 / real(nvox)
			else if (PTYPE(vp) == P_SUM)
			    oline[i] = oline[i] + ival
			else if (PTYPE(vp) == P_LASTONLY)
			    if (ival > 0.0)
				oline[i] = ival
		    }
		}
end



# VTRANSMIT -- Compute the intensities of each output image pixel in the
# current line as a function of its existing intensity plus the emission
# and absorption from each contributing voxel.

procedure vtransmitr (inbuf,nx,ny,nz,nh, px1,px2, iline,iband,nvox, oline, vp)
real	inbuf[nx,ny,nz,nh]	# Input data buffer for current set of yz slices
int	nx,ny,nz,nh		# Dimensions of current input buffer
int	px1,px2			# Range of columns in current yz slice set
int	iline[nvox]		# Input image lines for current projection ray
int	iband[nvox]		# Input image bands for current projection ray
int	nvox			# Number of voxels in current projection column
real	oline[ARB]		# output image line buffer
pointer	vp			# Volume projection descriptor

bool	use_both

int	i, vox, opelem, intelem, frontvox, backvox
real	amin, amax, vox_op, vox_int, ival, attenuate, vimin, ifac, ofac, distwt

begin
	# Dereference most frequently used structure elements.
	amin = AMIN(vp)
	amax = AMAX(vp)
	vimin = VIMIN(vp)

	intelem = 1
	opelem = OPACELEM(vp)
	if (nh > 1) {
	    use_both = true
	    if (opelem == 1)
		intelem = 2
	    else if (IS_INDEFI(opelem))
		opelem = 2
	} else {
	    use_both = false
	    opelem = 1
	}


	# Set up for opacity, intensity, or both.
	ifac = (IIMAX(vp) - IIMIN(vp)) / (VIMAX(vp) - vimin)
	if (PTYPE(vp) == P_ATTENUATE || use_both)
	    ofac = (amax - amin) / (OMAX(vp) - OMIN(vp))

	# Since we are in memory anyway, it is more convenient to traverse
	# the columns in the outer loop and the voxels from different bands
	# and lines in the inner loop.  This is necessary when distance
	# weighting and the distance cutoff option is on (we need to know
	# the range of usable voxels in a given column before projecting).

	if (PTYPE(vp) == P_INVDISPOW || PTYPE(vp) == P_MODN)
	    do i = px1, px2 {
		if (DISCUTOFF(vp) == NO) {
		    frontvox = nvox
		    backvox = 1
		} else {
		    frontvox = 1
		    backvox = nvox
		    do vox = 1, nvox {
			vox_int = inbuf[(i-px1+1),iline[vox],iband[vox],intelem]
			if (vimin <= vox_int && vox_int < VIMAX(vp)) {
			    frontvox = max (frontvox, vox)
			    backvox = min (backvox, vox)
			}
		    }
		}
		if (frontvox - backvox < 0)
		    next
		do vox = backvox, frontvox {
		    distwt = (real(vox-backvox+1) /
			real(frontvox-backvox+1)) ** DISPOWER(vp)

		    # Opacity transformation function.
		    if (use_both) {
			vox_op = inbuf[(i-px1+1), iline[vox], iband[vox],
			    opelem] * OSCALE(vp)
			if (vox_op < OMIN(vp))
			    attenuate = amax
			else if (OMIN(vp) <= vox_op && vox_op < OMAX(vp))
			    attenuate = amax - (vox_op - OMIN(vp)) * ofac
			else
			    attenuate = amin
			oline[i] = oline[i] * attenuate
		    }

		    # Intensity transformation function.
		    vox_int = inbuf[(i-px1+1),iline[vox],iband[vox],
			intelem]
		    if (vox_int < vimin)
			ival = IIMIN(vp)
		    else if (vimin <= vox_int && vox_int < VIMAX(vp))
			ival = IIMIN(vp) + (vox_int - vimin) * ifac
		    else
			ival = IIMAX(vp)

		    if (PTYPE(vp) == P_INVDISPOW)
			oline[i] = oline[i] + ival * distwt
		    else if (mod (int (ival/(IIMAX(vp)-IIMIN(vp)) * 100.0),
			    MODN(vp)) == 0)
			    oline[i] = oline[i] + ival * distwt
		}
	    }
	else
	    do i = px1, px2
		do vox = 1, nvox {
		    # Opacity transformation function.
		    if (PTYPE(vp) == P_ATTENUATE || use_both) {
			vox_op = inbuf[(i-px1+1), iline[vox], iband[vox],
			    opelem] * OSCALE(vp)
			if (vox_op < OMIN(vp))
			    attenuate = amax
			else if (OMIN(vp) <= vox_op && vox_op < OMAX(vp))
			    attenuate = amax - (vox_op - OMIN(vp)) * ofac
			else
			    attenuate = amin
			oline[i] = oline[i] * attenuate
		    }

		    # Intensity transformation function.
		    if (PTYPE(vp) != P_ATTENUATE || use_both) {
			vox_int = inbuf[(i-px1+1),iline[vox],iband[vox],
			    intelem]
			if (vox_int < vimin)
			    ival = IIMIN(vp)
			else if (vimin <= vox_int && vox_int < VIMAX(vp))
			    ival = IIMIN(vp) + (vox_int - vimin) * ifac
			else
			    ival = IIMAX(vp)

			if (PTYPE(vp) == P_AVERAGE)
			    oline[i] = oline[i] + ival * 1.0 / real(nvox)
			else if (PTYPE(vp) == P_SUM)
			    oline[i] = oline[i] + ival
			else if (PTYPE(vp) == P_LASTONLY)
			    if (ival > 0.0)
				oline[i] = ival
		    }
		}
end



# VTRANSMIT -- Compute the intensities of each output image pixel in the
# current line as a function of its existing intensity plus the emission
# and absorption from each contributing voxel.

procedure vtransmitd (inbuf,nx,ny,nz,nh, px1,px2, iline,iband,nvox, oline, vp)
double	inbuf[nx,ny,nz,nh]	# Input data buffer for current set of yz slices
int	nx,ny,nz,nh		# Dimensions of current input buffer
int	px1,px2			# Range of columns in current yz slice set
int	iline[nvox]		# Input image lines for current projection ray
int	iband[nvox]		# Input image bands for current projection ray
int	nvox			# Number of voxels in current projection column
real	oline[ARB]		# output image line buffer
pointer	vp			# Volume projection descriptor

bool	use_both

int	i, vox, opelem, intelem, frontvox, backvox
real	amin, amax, vox_op, vox_int, ival, attenuate, vimin, ifac, ofac, distwt

begin
	# Dereference most frequently used structure elements.
	amin = AMIN(vp)
	amax = AMAX(vp)
	vimin = VIMIN(vp)

	intelem = 1
	opelem = OPACELEM(vp)
	if (nh > 1) {
	    use_both = true
	    if (opelem == 1)
		intelem = 2
	    else if (IS_INDEFI(opelem))
		opelem = 2
	} else {
	    use_both = false
	    opelem = 1
	}


	# Set up for opacity, intensity, or both.
	ifac = (IIMAX(vp) - IIMIN(vp)) / (VIMAX(vp) - vimin)
	if (PTYPE(vp) == P_ATTENUATE || use_both)
	    ofac = (amax - amin) / (OMAX(vp) - OMIN(vp))

	# Since we are in memory anyway, it is more convenient to traverse
	# the columns in the outer loop and the voxels from different bands
	# and lines in the inner loop.  This is necessary when distance
	# weighting and the distance cutoff option is on (we need to know
	# the range of usable voxels in a given column before projecting).

	if (PTYPE(vp) == P_INVDISPOW || PTYPE(vp) == P_MODN)
	    do i = px1, px2 {
		if (DISCUTOFF(vp) == NO) {
		    frontvox = nvox
		    backvox = 1
		} else {
		    frontvox = 1
		    backvox = nvox
		    do vox = 1, nvox {
			vox_int = inbuf[(i-px1+1),iline[vox],iband[vox],intelem]
			if (vimin <= vox_int && vox_int < VIMAX(vp)) {
			    frontvox = max (frontvox, vox)
			    backvox = min (backvox, vox)
			}
		    }
		}
		if (frontvox - backvox < 0)
		    next
		do vox = backvox, frontvox {
		    distwt = (real(vox-backvox+1) /
			real(frontvox-backvox+1)) ** DISPOWER(vp)

		    # Opacity transformation function.
		    if (use_both) {
			vox_op = inbuf[(i-px1+1), iline[vox], iband[vox],
			    opelem] * OSCALE(vp)
			if (vox_op < OMIN(vp))
			    attenuate = amax
			else if (OMIN(vp) <= vox_op && vox_op < OMAX(vp))
			    attenuate = amax - (vox_op - OMIN(vp)) * ofac
			else
			    attenuate = amin
			oline[i] = oline[i] * attenuate
		    }

		    # Intensity transformation function.
		    vox_int = inbuf[(i-px1+1),iline[vox],iband[vox],
			intelem]
		    if (vox_int < vimin)
			ival = IIMIN(vp)
		    else if (vimin <= vox_int && vox_int < VIMAX(vp))
			ival = IIMIN(vp) + (vox_int - vimin) * ifac
		    else
			ival = IIMAX(vp)

		    if (PTYPE(vp) == P_INVDISPOW)
			oline[i] = oline[i] + ival * distwt
		    else if (mod (int (ival/(IIMAX(vp)-IIMIN(vp)) * 100.0),
			    MODN(vp)) == 0)
			    oline[i] = oline[i] + ival * distwt
		}
	    }
	else
	    do i = px1, px2
		do vox = 1, nvox {
		    # Opacity transformation function.
		    if (PTYPE(vp) == P_ATTENUATE || use_both) {
			vox_op = inbuf[(i-px1+1), iline[vox], iband[vox],
			    opelem] * OSCALE(vp)
			if (vox_op < OMIN(vp))
			    attenuate = amax
			else if (OMIN(vp) <= vox_op && vox_op < OMAX(vp))
			    attenuate = amax - (vox_op - OMIN(vp)) * ofac
			else
			    attenuate = amin
			oline[i] = oline[i] * attenuate
		    }

		    # Intensity transformation function.
		    if (PTYPE(vp) != P_ATTENUATE || use_both) {
			vox_int = inbuf[(i-px1+1),iline[vox],iband[vox],
			    intelem]
			if (vox_int < vimin)
			    ival = IIMIN(vp)
			else if (vimin <= vox_int && vox_int < VIMAX(vp))
			    ival = IIMIN(vp) + (vox_int - vimin) * ifac
			else
			    ival = IIMAX(vp)

			if (PTYPE(vp) == P_AVERAGE)
			    oline[i] = oline[i] + ival * 1.0 / real(nvox)
			else if (PTYPE(vp) == P_SUM)
			    oline[i] = oline[i] + ival
			else if (PTYPE(vp) == P_LASTONLY)
			    if (ival > 0.0)
				oline[i] = ival
		    }
		}
end



# VTRANSMIT -- Compute the intensities of each output image pixel in the
# current line as a function of its existing intensity plus the emission
# and absorption from each contributing voxel.

procedure vtransmitx (inbuf,nx,ny,nz,nh, px1,px2, iline,iband,nvox, oline, vp)
complex	inbuf[nx,ny,nz,nh]	# Input data buffer for current set of yz slices
int	nx,ny,nz,nh		# Dimensions of current input buffer
int	px1,px2			# Range of columns in current yz slice set
int	iline[nvox]		# Input image lines for current projection ray
int	iband[nvox]		# Input image bands for current projection ray
int	nvox			# Number of voxels in current projection column
real	oline[ARB]		# output image line buffer
pointer	vp			# Volume projection descriptor

bool	use_both

int	i, vox, opelem, intelem, frontvox, backvox
real	amin, amax, vox_op, vox_int, ival, attenuate, vimin, ifac, ofac, distwt

begin
	# Dereference most frequently used structure elements.
	amin = AMIN(vp)
	amax = AMAX(vp)
	vimin = VIMIN(vp)

	intelem = 1
	opelem = OPACELEM(vp)
	if (nh > 1) {
	    use_both = true
	    if (opelem == 1)
		intelem = 2
	    else if (IS_INDEFI(opelem))
		opelem = 2
	} else {
	    use_both = false
	    opelem = 1
	}


	# Set up for opacity, intensity, or both.
	ifac = (IIMAX(vp) - IIMIN(vp)) / (VIMAX(vp) - vimin)
	if (PTYPE(vp) == P_ATTENUATE || use_both)
	    ofac = (amax - amin) / (OMAX(vp) - OMIN(vp))

	# Since we are in memory anyway, it is more convenient to traverse
	# the columns in the outer loop and the voxels from different bands
	# and lines in the inner loop.  This is necessary when distance
	# weighting and the distance cutoff option is on (we need to know
	# the range of usable voxels in a given column before projecting).

	if (PTYPE(vp) == P_INVDISPOW || PTYPE(vp) == P_MODN)
	    do i = px1, px2 {
		if (DISCUTOFF(vp) == NO) {
		    frontvox = nvox
		    backvox = 1
		} else {
		    frontvox = 1
		    backvox = nvox
		    do vox = 1, nvox {
			vox_int = inbuf[(i-px1+1),iline[vox],iband[vox],intelem]
			if (vimin <= vox_int && vox_int < VIMAX(vp)) {
			    frontvox = max (frontvox, vox)
			    backvox = min (backvox, vox)
			}
		    }
		}
		if (frontvox - backvox < 0)
		    next
		do vox = backvox, frontvox {
		    distwt = (real(vox-backvox+1) /
			real(frontvox-backvox+1)) ** DISPOWER(vp)

		    # Opacity transformation function.
		    if (use_both) {
			vox_op = inbuf[(i-px1+1), iline[vox], iband[vox],
			    opelem] * OSCALE(vp)
			if (vox_op < OMIN(vp))
			    attenuate = amax
			else if (OMIN(vp) <= vox_op && vox_op < OMAX(vp))
			    attenuate = amax - (vox_op - OMIN(vp)) * ofac
			else
			    attenuate = amin
			oline[i] = oline[i] * attenuate
		    }

		    # Intensity transformation function.
		    vox_int = inbuf[(i-px1+1),iline[vox],iband[vox],
			intelem]
		    if (vox_int < vimin)
			ival = IIMIN(vp)
		    else if (vimin <= vox_int && vox_int < VIMAX(vp))
			ival = IIMIN(vp) + (vox_int - vimin) * ifac
		    else
			ival = IIMAX(vp)

		    if (PTYPE(vp) == P_INVDISPOW)
			oline[i] = oline[i] + ival * distwt
		    else if (mod (int (ival/(IIMAX(vp)-IIMIN(vp)) * 100.0),
			    MODN(vp)) == 0)
			    oline[i] = oline[i] + ival * distwt
		}
	    }
	else
	    do i = px1, px2
		do vox = 1, nvox {
		    # Opacity transformation function.
		    if (PTYPE(vp) == P_ATTENUATE || use_both) {
			vox_op = inbuf[(i-px1+1), iline[vox], iband[vox],
			    opelem] * OSCALE(vp)
			if (vox_op < OMIN(vp))
			    attenuate = amax
			else if (OMIN(vp) <= vox_op && vox_op < OMAX(vp))
			    attenuate = amax - (vox_op - OMIN(vp)) * ofac
			else
			    attenuate = amin
			oline[i] = oline[i] * attenuate
		    }

		    # Intensity transformation function.
		    if (PTYPE(vp) != P_ATTENUATE || use_both) {
			vox_int = inbuf[(i-px1+1),iline[vox],iband[vox],
			    intelem]
			if (vox_int < vimin)
			    ival = IIMIN(vp)
			else if (vimin <= vox_int && vox_int < VIMAX(vp))
			    ival = IIMIN(vp) + (vox_int - vimin) * ifac
			else
			    ival = IIMAX(vp)

			if (PTYPE(vp) == P_AVERAGE)
			    oline[i] = oline[i] + ival * 1.0 / real(nvox)
			else if (PTYPE(vp) == P_SUM)
			    oline[i] = oline[i] + ival
			else if (PTYPE(vp) == P_LASTONLY)
			    if (ival > 0.0)
				oline[i] = ival
		    }
		}
end


