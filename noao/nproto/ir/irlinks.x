include "iralign.h"

# IR_LINKS -- Procedure to compute the shifts for each subraster.

int procedure ir_links (cl, xrshift, yrshift, xcshift, ycshift, nrshift,
	ncshift, ncols, nrows, nxrsub, nyrsub, nxsub, nysub, nxoverlap,
	nyoverlap, order)

int	cl			# coordinate list descriptor
real	xrshift[nxsub,ARB]	# x row shifts
real	yrshift[nxsub,ARB]	# y row shifts
real	xcshift[nxsub,ARB]	# x column shifts
real	ycshift[nxsub,ARB]	# y column shifts
int	nrshift[nxsub,ARB]	# number of row shifts
int	ncshift[nxsub,ARB]	# number of column shifts
int	ncols			# number of columns per subraster
int	nrows			# number of rows per subraster
int	nxrsub			# column index of reference subraster
int	nyrsub			# row index of reference subraster
int	nxsub			# number of subrasters in x
int	nysub			# number of subrasters in y
int	nxoverlap		# number of columns of overlap
int	nyoverlap		# number of rows of overlap
int	order			# row or column order

int	i, j, nxsize, nysize, ilimit, olimit, nshifts
pointer	sp, xcolavg, ycolavg, xrowavg, yrowavg, nrowavg, ncolavg
real	isign, jsign, xrmed, yrmed, xcmed, ycmed
int	ir_decode_shifts()
real	irmedr()

begin
	# Allocate temporary space.
	if (order == IR_COLUMN) {
	    ilimit = nysub
	    olimit = nxsub
	} else {
	    ilimit = nxsub
	    olimit = nysub
	}

	# Clear the shift arrays.
	call aclrr (xrshift, nxsub * nysub)
	call aclrr (yrshift, nxsub * nysub)
	call aclrr (xcshift, nxsub * nysub)
	call aclrr (ycshift, nxsub * nysub)
	call aclri (nrshift, nxsub * nysub)
	call aclri (ncshift, nxsub * nysub)

	# Accumulate the shifts.
	nxsize = ncols - nxoverlap
	nysize = nrows - nyoverlap
	nshifts = ir_decode_shifts (cl, xrshift, yrshift, nrshift, xcshift,
	    ycshift, ncshift, nxsub, nysub, nxrsub, nyrsub, nxoverlap,
	    nyoverlap, nxsize, nysize)
	if (nshifts == 0)
	    return (0)

	# Allocate working space.
	call smark (sp)
	call salloc (xcolavg, olimit, TY_REAL)
	call salloc (ycolavg, olimit, TY_REAL)
	call salloc (ncolavg, olimit, TY_INT)
	call salloc (xrowavg, olimit, TY_REAL)
	call salloc (yrowavg, olimit, TY_REAL)
	call salloc (nrowavg, olimit, TY_INT)

	# Clear the accumulator arrays.
	call aclrr (Memr[xcolavg], olimit)
	call aclrr (Memr[ycolavg], olimit)
	call aclri (Memi[ncolavg], olimit)
	call aclrr (Memr[xrowavg], olimit)
	call aclrr (Memr[yrowavg], olimit)
	call aclri (Memi[nrowavg], olimit)

	# Compute the row or column sums.
	if (order == IR_COLUMN) {
	    do i = 1, nxsub {
	        do j = 1, nysub {
		    if (nrshift[i,j] > 0) {
		        Memr[xrowavg+i-1] = Memr[xrowavg+i-1] +
			     abs (xrshift[i,j])
		        Memr[yrowavg+i-1] = Memr[yrowavg+i-1] +
			    abs (yrshift[i,j])
		        Memi[nrowavg+i-1] = Memi[nrowavg+i-1] + 1
		    } 
		    if (ncshift[i,j] > 0) {
		        Memr[xcolavg+i-1] = Memr[xcolavg+i-1] +
			    abs (xcshift[i,j])
		        Memr[ycolavg+i-1] = Memr[ycolavg+i-1] +
			    abs (ycshift[i,j])
		        Memi[ncolavg+i-1] = Memi[ncolavg+i-1] + 1
		    }
	        }
	    }
	} else {
	    do i = 1, nysub {
	        do j = 1, nxsub {
		    if (nrshift[j,i] > 0) {
		        Memr[xrowavg+i-1] = Memr[xrowavg+i-1] +
			    abs (xrshift[j,i])
		        Memr[yrowavg+i-1] = Memr[yrowavg+i-1] +
			    abs (yrshift[j,i])
		        Memi[nrowavg+i-1] = Memi[nrowavg+i-1] + 1
		    } 
		    if (ncshift[j,i] > 0) {
		        Memr[xcolavg+i-1] = Memr[xcolavg+i-1] +
			    abs (xcshift[j,i])
		        Memr[ycolavg+i-1] = Memr[ycolavg+i-1] +
			    abs (ycshift[j,i])
		        Memi[ncolavg+i-1] = Memi[ncolavg+i-1] + 1
		    }
	        }
	    }
	}

	# Compute the averages.
	do i = 1, olimit {
	    if (Memi[nrowavg+i-1] > 0) {
	        Memr[xrowavg+i-1] = Memr[xrowavg+i-1] / Memi[nrowavg+i-1]
		Memr[yrowavg+i-1] = Memr[yrowavg+i-1] / Memi[nrowavg+i-1]
	    }
	    if (Memi[ncolavg+i-1] > 0) {
		Memr[xcolavg+i-1] = Memr[xcolavg+i-1] / Memi[ncolavg+i-1]
		Memr[ycolavg+i-1] = Memr[ycolavg+i-1] / Memi[ncolavg+i-1]
	    }
	 }

	 # Compute the medians of the row and column averages.
	 xrmed = irmedr (Memr[xrowavg], Memi[nrowavg], olimit)
	 yrmed = irmedr (Memr[yrowavg], Memi[nrowavg], olimit)
	 xcmed = irmedr (Memr[xcolavg], Memi[ncolavg], olimit)
	 ycmed = irmedr (Memr[ycolavg], Memi[ncolavg], olimit)

	# Use the average shifts for subrasters with no information.
	do j = 1, nysub {

	    if (j == nyrsub)
		jsign = 0.0
	    else if (j < nyrsub)
		jsign = 1.0
	    else
		jsign = -1.0

	    do i = 1, nxsub {

		if (i == nxrsub)
		    isign = 0.0
		else if (i < nxrsub)
		    isign = 1.0
		else
		    isign = -1.0

		if (nrshift[i,j] <= 0) {
		    if (Memi[nrowavg+i-1] <= 0) {
			xrshift[i,j] = isign * xrmed
			yrshift[i,j] = jsign * yrmed
		    } else if (order == IR_COLUMN) {
		        xrshift[i,j] = isign * Memr[xrowavg+i-1]
		        yrshift[i,j] = jsign * Memr[yrowavg+i-1]
		    } else {
		        xrshift[i,j] = isign * Memr[xrowavg+j-1]
		        yrshift[i,j] = jsign * Memr[yrowavg+j-1]
		    }
		}

		if (ncshift[i,j] <= 0) {
		    if (Memi[ncolavg+i-1] <= 0) {
		        xcshift[i,j] = isign * xcmed
		        ycshift[i,j] = jsign * ycmed
		    } else if (order == IR_COLUMN) {
		        xcshift[i,j] = isign * Memr[xcolavg+i-1]
		        ycshift[i,j] = jsign * Memr[ycolavg+i-1]
		    } else {
		        xcshift[i,j] = isign * Memr[xcolavg+j-1]
		        ycshift[i,j] = jsign * Memr[ycolavg+j-1]
		    }
		}
	    }
	}

	call sfree (sp)
	return (nshifts)
end


# IR_DECODE_SHIFTS -- Procedure to accumulate shifts for each subraster.

int procedure ir_decode_shifts (cl, xrshift, yrshift, nrshift, xcshift,
	ycshift, ncshift, nxsub, nysub, nxrsub, nyrsub, nxoverlap,
	nyoverlap, nxsize, nysize)

int	cl			# coordinate list descriptor
real	xrshift[nxsub,ARB]	# x row shifts
real	yrshift[nxsub,ARB]	# y row shifts
int	nrshift[nxsub,ARB]	# number of row shifts
real	xcshift[nxsub,ARB]	# x column shifts
real	ycshift[nxsub,ARB]	# y column shifts
int	ncshift[nxsub,ARB]	# number of column shifts
int	nxsub			# number of subrasters in x
int	nysub			# number of subrasters in y
int	nxrsub			# column index of reference subraster
int	nyrsub			# row index of reference subraster
int	nxoverlap		# number of columns of overlap
int	nyoverlap		# number of rows of overlap
int	nxsize			# size of unoverlapped region
int	nysize			# size of unoverlapped region

int	i, j, nx1, ny1, nx2, ny2, r21, r22, stat, nshifts
real	x1, y1, x2, y2, xdif, xdifm, ydif, ydifm
int	fscan(), nscan()

begin
	nshifts = 0
	while (fscan (cl) != EOF) {

	    # Get the first coordinate pair.
	    call gargr (x1)
	    call gargr (y1)
	    if (nscan () != 2)
		next

	    # Compute which subraster 1 belongs to.
	    if (mod (int (x1), nxsize) == 0)
		nx1 = int (x1) / nxsize
	    else
	        nx1 = int (x1) / nxsize + 1

	    if (mod (int (y1), nysize) == 0)
	        ny1 = int (y1) / nysize
	    else
	    	ny1 = int (y1) / nysize + 1

	    # Get the second coordinate pair.
	    repeat {

		stat = fscan (cl)
		if (stat == EOF)
		    break
		call gargr (x2)
		call gargr (y2)

	        # Compute which subraster 2 belongs to.
		if (nscan () == 2) {
	            if (mod (int (x2), nxsize) == 0)
		        nx2 = int (x2) / nxsize
	            else
	                nx2 = int (x2) / nxsize + 1
	            if (mod (int (y2), nysize) == 0)
		        ny2 = int (y2) / nysize
	            else
	                ny2 = int (y2) / nysize + 1
		}

	    } until (nscan () == 2)
	    if (stat == EOF || nscan() != 2)
		break

	    r21 = (nx1 - nxrsub) ** 2 + (ny1 - nyrsub) ** 2
	    r22 = (nx2 - nxrsub) ** 2 + (ny2 - nyrsub) ** 2

	    # Illegal shift
	    if (r21 == r22)
		next

	    # Compute the shift for the first subraster.
	    else if (r21 > r22) {

		xdif = x2 - x1
		if (nxoverlap < 0) {
		    if (xdif < 0.0)
		        xdifm = xdif - nxoverlap
		    else if (xdif > 0.0)
		        xdifm = xdif + nxoverlap
		} else
		    xdifm = xdif

		ydif = y2 - y1
		if (nyoverlap < 0) {
		    if (ydif < 0.0)
			ydifm = ydif - nyoverlap
		    else if (ydif > 0.0)
			ydifm = ydif + nyoverlap
		} else
		    ydifm = ydif

		if (nx1 == nx2) {
		    xcshift[nx1,ny1] = xcshift[nx1,ny1] + xdif
		    ycshift[nx1,ny1] = ycshift[nx1,ny1] + ydifm
		    ncshift[nx1,ny1] = ncshift[nx1,ny1] + 1
		} else if (ny1 == ny2) {
		    xrshift[nx1,ny1] = xrshift[nx1,ny1] + xdifm
		    yrshift[nx1,ny1] = yrshift[nx1,ny1] + ydif
		    nrshift[nx1,ny1] = nrshift[nx1,ny1] + 1
		} else
		    next

	    # Compute the shift for the second subraster.
	    } else {

		xdif = x1 - x2
		if (nxoverlap < 0) {
		    if (xdif < 0.0)
		        xdifm = xdif - nxoverlap
		    else if (xdif > 0.0)
		        xdifm = xdif + nxoverlap
		} else
		    xdifm = xdif

		ydif = y1 - y2
		if (nyoverlap < 0) {
		    if (ydif < 0.0)
			ydifm = ydif - nyoverlap
		    else if (ydif > 0.0)
			ydifm = ydif + nyoverlap
		} else
		    ydifm = ydif

		if (nx1 == nx2) {
		    xcshift[nx2,ny2] = xcshift[nx2,ny2] + xdif
		    ycshift[nx2,ny2] = ycshift[nx2,ny2] + ydifm
		    ncshift[nx2,ny2] = ncshift[nx2,ny2] + 1
		} else if (ny1 == ny2) {
		    xrshift[nx2,ny2] = xrshift[nx2,ny2] + xdifm
		    yrshift[nx2,ny2] = yrshift[nx2,ny2] + ydif
		    nrshift[nx2,ny2] = nrshift[nx2,ny2] + 1
		} else
		    next
	    }

	    nshifts = nshifts + 1
	}

	# Compute the final shifts.
	do j = 1, nysub {
	    do i = 1, nxsub {
		if (nrshift[i,j] > 0) {
		    xrshift[i,j] = xrshift[i,j] / nrshift[i,j]
		    yrshift[i,j] = yrshift[i,j] / nrshift[i,j]
		}
		if (ncshift[i,j] > 0) {
		    xcshift[i,j] = xcshift[i,j] / ncshift[i,j]
		    ycshift[i,j] = ycshift[i,j] / ncshift[i,j]
		}
	    }
	}

	return (nshifts)
end


# IR_CLINKS -- Procedure to compute the shifts for each subraster.

int procedure ir_clinks (xrshift, yrshift, xcshift, ycshift, nxrsub, nyrsub,
	nxsub, nysub, xshift, yshift)

real	xrshift[nxsub,ARB]	# x row shifts
real	yrshift[nxsub,ARB]	# y row shifts
real	xcshift[nxsub,ARB]	# x column shifts
real	ycshift[nxsub,ARB]	# y column shifts
int	nxrsub			# x index of reference subraster
int	nyrsub			# y index of reference subraster
int	nxsub			# number of subrasters in x direction
int	nysub			# number of subrasters in y direction
real	xshift			# xshift of the coordinates
real	yshift			# yshift of the coordinates

int	i, j, isign, jsign

begin
	do j = 1, nysub {
	    if (j == nyrsub)
		jsign = 0
	    else if (j < nyrsub)
		jsign = 1
	    else
		jsign = -1

	    do i = 1, nxsub {
	        if (i == nxrsub)
		    isign = 0
	        else if (i < nxrsub)
		    isign = 1
	        else
		    isign = -1

		xrshift[i,j] = isign * abs (xshift)
		yrshift[i,j] = 0.0
		xcshift[i,j] = 0.0 
		ycshift[i,j] = jsign * abs (yshift)
	    }
	}

	return (1)
end


# IR_FLINKS -- Routine to fetch the shifts directly

int procedure ir_flinks (cl, deltax, deltay, deltai, max_nshifts)

int	cl		# shifts file descriptor
real	deltax[ARB]	# x shifts
real	deltay[ARB]	# y shifts
real	deltai[ARB]	# intensity shifts
int	max_nshifts	# maximum number of shifts

int	nshifts
int	fscan(), nscan()

begin
	nshifts = 0
	while ((fscan (cl) != EOF) && (nshifts < max_nshifts)) {
	    call gargr (deltax[nshifts+1])
	    call gargr (deltay[nshifts+1])
	    call gargr (deltai[nshifts+1])
	    if (nscan() < 2)
		next
	    if (nscan() < 3)
		deltai[nshifts+1] = 0.0
	    nshifts = nshifts + 1
	}

	return (nshifts)
end


# IR_MKSHIFT -- Routine to compute the total shift for each subraster.

procedure ir_mkshift (xrshift, yrshift, xcshift, ycshift, nxsub, nysub,
        xsubindex, ysubindex, nxrsub, nyrsub, order, deltax, deltay)

real	xrshift[nxsub,ARB]	# x row shifts
real	yrshift[nxsub,ARB]	# y row shifts
real	xcshift[nxsub,ARB]	# x column shifts
real	ycshift[nxsub,ARB]	# y column shifts
int	nxsub			# number of subrasters in x direction
int	nysub			# number of subrasters in y direction
int	xsubindex		# x index of the subraster
int	ysubindex		# y index of the subraster
int	nxrsub			# x index of reference subraster
int	nyrsub			# y index of reference subraster
int	order			# row or column order
real	deltax			# total x shift
real	deltay			# total y shift

int	j

begin
	deltax = 0.0
	deltay = 0.0

	if (order == IR_COLUMN) {
	    if (ysubindex < nyrsub)
		do j = ysubindex, nyrsub - 1 {
		    deltax = deltax + xcshift[xsubindex,j]
		    deltay = deltay + ycshift[xsubindex,j]
		}
	    else if (ysubindex > nyrsub)
		do j = nyrsub + 1, ysubindex {
		    deltax = deltax + xcshift[xsubindex,j]
		    deltay = deltay + ycshift[xsubindex,j]
		}
	    if (xsubindex < nxrsub)
		do j = xsubindex, nxrsub - 1 {
		    deltax = deltax + xrshift[j,nyrsub]
		    deltay = deltay + yrshift[j,nyrsub]
		}
	    else if (xsubindex > nxrsub)
		do j = nxrsub + 1, xsubindex {
		    deltax = deltax + xrshift[j,nyrsub]
		    deltay = deltay + yrshift[j,nyrsub]
		}
	} else {
	    if (xsubindex < nxrsub)
		do j = xsubindex, nxrsub - 1{
		    deltax = deltax + xrshift[j,ysubindex]
		    deltay = deltay + yrshift[j,ysubindex]
		}
	    else if (xsubindex > nxrsub)
		do j = nxrsub + 1, xsubindex {
		    deltax = deltax + xrshift[j,ysubindex]
		    deltay = deltay + yrshift[j,ysubindex]
		}
	    if (ysubindex < nyrsub)
		do j = ysubindex, nyrsub - 1 {
		    deltax = deltax + xcshift[nxrsub,j]
		    deltay = deltay + ycshift[nxrsub,j]
		}
	    else if (ysubindex > nyrsub)
		do j = nyrsub + 1, ysubindex {
		    deltax = deltax + xcshift[nxrsub,j]
		    deltay = deltay + ycshift[nxrsub,j]
		}
	}
end
