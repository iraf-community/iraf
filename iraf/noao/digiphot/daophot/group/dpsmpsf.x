include <mach.h>
include "../lib/daophotdef.h"

define	NSEC	4
define	IRMIN	4

# DP_SMPSF -- Smooth the psf before grouping.

procedure dp_smpsf (dao)

pointer	dao			# pointer to the daophot strucuture

int	k, icenter, irmax, nexpand
pointer	psffit, sum, high, low, n
real	rmax

begin
	# Get some pointers.
	psffit = DP_PSFFIT(dao)

	# Get some constants.
	icenter = (DP_PSFSIZE(psffit) + 1) / 2
	rmax = .7071068 * real (DP_PSFSIZE(psffit) - 1)
	irmax = int (rmax + 1.0e-5)
	nexpand = DP_NVLTABLE(psffit) + DP_NFEXTABLE(psffit)

	# Allocate working memory.
	call malloc (sum, NSEC * irmax, TY_REAL)
	call malloc (high, NSEC * irmax, TY_REAL)
	call malloc (low, NSEC * irmax, TY_REAL)
	call malloc (n, NSEC * irmax, TY_INT)

	# Do the smoothing.
	do k = 1, nexpand {

	    # Initialize.
	    call aclrr (Memr[sum], NSEC * irmax)
	    call amovkr (-MAX_REAL, Memr[high], NSEC * irmax)
	    call amovkr (MAX_REAL, Memr[low], NSEC * irmax)
	    call aclri (Memi[n], NSEC * irmax)

	    # Acumulate.
	    call dp_smaccum (Memr[DP_PSFLUT(psffit)], DP_PSFSIZE(psffit),
		DP_PSFSIZE(psffit), Memr[sum], Memr[low], Memr[high], Memi[n],
		NSEC, IRMIN, icenter, rmax, k)

	    # Normalize.
	    call dp_smnorm (Memr[sum], Memr[low], Memr[high], Memi[n], NSEC,
	        IRMIN, irmax)

	    # Smooth.
	    call dp_smo (Memr[DP_PSFLUT(psffit)], DP_PSFSIZE(psffit),
		DP_PSFSIZE(psffit), Memr[sum], NSEC, IRMIN, icenter, rmax, k)
	}

	call mfree (sum, TY_REAL)
	call mfree (low, TY_REAL)
	call mfree (high, TY_REAL)
	call mfree (n, TY_INT)
end


# DP_SMACCUM -- Accumulate the sums and limits

procedure dp_smaccum (psflut, nxpsf, nypsf, sum, low, high, n, nsec, irmin,
	icenter, rmax, k)

real	psflut[nxpsf,nypsf,ARB]		# the psf lookup table
int	nxpsf, nypsf			# size of the psf lookup table
real	sum[nsec,ARB]			# array of sums
real	low[nsec,ARB]			# array of low values
real	high[nsec,ARB]			# array of high values
int	n[nsec,ARB]			# array of number of points
int	nsec				# dimension of sum arrays
int	irmin				# number of sums
int	icenter				# center of the array
real	rmax				# max radius
int	k				# third dimension array index

int	i, j, idx, idy, is, ir
real	dxsq, dysq, r
int	dp_isctr()

begin
	do j = 1, nypsf {
	    idy = j - icenter
	    dysq = idy ** 2
	    do i = 1, nxpsf {
		idx = i - icenter
		dxsq = idx ** 2
		r = sqrt (dxsq + dysq)
		if (r > rmax)
		    next
		ir = int (r + 1.0e-5)
		if (ir < irmin)
		    next
		is = dp_isctr (idx, idy)
		sum[is,ir] = sum[is,ir] + psflut[i,j,k]
		if (psflut[i,j,k] > high[is,ir])
		    high[is,ir] = psflut[i,j,k]
		if (psflut[i,j,k] < low[is,ir])
		    low[is,ir] = psflut[i,j,k]
		n[is,ir] = n[is,ir] + 1
	    }
	}
end


# DP_SMNORM -- Normalize the sum

procedure dp_smnorm (sum, low, high, n, nsec, irmin, irmax)

real	sum[nsec,ARB]		# array of sums
real	low[nsec,ARB]		# array of low values
real	high[nsec,ARB]		# array of high values
int	n[nsec,ARB]		# array of counter
int	nsec			# array dimension
int	irmin			# radius index
int	irmax			# maximum radius index

int	ir, is

begin
	do ir = irmin, irmax {
	    do is = 1, nsec {
		if (n[is,ir] > 2)
		    sum[is,ir] = (sum[is,ir] - high[is,ir] - low[is,ir]) /
			(n[is,ir] - 2)
	    }
	}
end


# DP_SMO -- Do the actual smoothing.

procedure dp_smo (psflut, nxpsf, nypsf, sum, nrec, irmin, icenter, rmax, k)

real	psflut[nxpsf,nypsf,ARB]		# the lookup table
int	nxpsf, nypsf			# size of the psf lookup table
real	sum[nrec,ARB]			# array of sums
int	nrec				# dimension of sum array
int	irmin				# min radius index
int	icenter				# index of center
real	rmax				# maximum radius
int	k				# index of third dimension

int	i, j, idx, idy, ir, is
real	dysq, r
int	dp_isctr()

begin
	do j = 1, nypsf {
	    idy = j - icenter
	    dysq = idy ** 2
	    do i = 1, nxpsf {
		idx = i - icenter
		r = sqrt (real (idx ** 2) + dysq)
		if (r > rmax)
		    next
		ir = int (r + 1.0e-5)
		if (ir < irmin)
		    next
		is = dp_isctr (idx, idy)
		psflut[i,j,k] = sum[is,ir]
	    }
	}
end


# DP_ISCTR -- Convert an index pair into a numbered sector from 1 to 4.

int procedure dp_isctr (i, j)

int	i		# first index
int	j		# second index

int	isctr

begin
	if (i > 0) {
	    isctr = 1
	} else if (i < 0) {
	    isctr = 3
	} else {
	    if (j <= 0)
		isctr = 1
	    else
		isctr = 3
	}

	if (j > 0) {
	    isctr = isctr + 1
	} else if (j == 0) {
	    if (i > 0)
		isctr = 2
	}

	return (isctr)
end
