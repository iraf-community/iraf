include <imhdr.h>
include "../lib/daophotdef.h"
include "../lib/psfdef.h"
include "../lib/psf.h"

# DP_GENPSF -- Generate the the PSF for this star

real procedure dp_genpsf (dao, psfim, oxstar, oystar, xstar, ystar,
	x1, y1, rel_bright, newpsf)

pointer	dao			# pointer to the daophot strucuture
pointer	psfim			# pointer to the output image
real	oxstar, oystar		# position relative to the image
real	xstar, ystar		# position of psf star
int	x1			# x limits in the original image
int	y1			# y limits in the original image
real	rel_bright		# mag relative to the first PSF star
int	newpsf			# newpsf

int	ncenter
pointer	psf, psffit
real	xzero, yzero, magnitude
real	coeff[SZ_PSFMATRIX]

begin
	# Initialize some pointers.
	psf = DP_PSF(dao)
	psffit = DP_PSFFIT(dao)

	# Initalize the PSF.
	if (newpsf == YES) {

	    # Set the constants.
	    DP_PSFMAG(psffit) = DP_CUR_PSFMAG(psf)
	    DP_XPSF(psffit) = DP_CUR_PSFX (psf)
	    DP_YPSF(psffit) = DP_CUR_PSFY (psf)

	    # Add information to the PSF image header.
	    call dp_headpars (psfim, dao)

	    # Delete all previous stars.
	    call dp_dpsf (psfim)

	    # Initialize the matrix.
	    call aclrr (Memr[DP_PSFMATRIX(psf)], SZ_PSFMATRIX * SZ_PSFMATRIX)
	    if (DP_VARPSF(dao) == YES)
	        call aclrr (Memr[DP_PSFLUT(psffit)], DP_PSFSIZE(psffit) *
		    DP_PSFSIZE(psffit) * 3)
	    else
	        call aclrr (Memr[DP_PSFLUT(psffit)], DP_PSFSIZE(psffit) *
		    DP_PSFSIZE(psffit))

	    magnitude = DP_PSFMAG(psffit)
	    DP_PSFNUMB(psf) = 1

	} else {

	    magnitude = DP_PSFMAG(psffit) - 2.5 * log10 (rel_bright)
	    DP_PSFNUMB (psf) = DP_PSFNUMB (psf) + 1
	}

	# Add the information for this star to the image header.
	call dp_apsf (psfim, DP_PSFNUMB (psf), DP_CUR_PSFID(psf),
	    DP_CUR_PSFX(psf), DP_CUR_PSFY(psf), magnitude)

	# Accumulate the matrix.
	coeff[1] = 1.0
	coeff[2] = oxstar -  DP_XPSF(psffit) # real (IM_LEN(im,1) + 1) / 2.0
	coeff[3] = oystar - DP_YPSF(psffit) # real (IM_LEN(im,2) + 1) / 2.0
	call dp_pmatrix (Memr[DP_PSFMATRIX(psf)], coeff, SZ_PSFMATRIX,
	    rel_bright)

	# Initialize.
	ncenter = (DP_PSFSIZE(psffit) + 1) / 2
	xzero = xstar - x1 + 1
	yzero = ystar - y1 + 1

	# Compute the lookup table.
	call dp_psflut (dao, Memr[DP_PSFLUT(psffit)], coeff, DP_PSFSIZE(psffit),
	    xzero, yzero, ncenter) 

	return (magnitude)
end


# DP_PMATRIX -- Compute the square psf matrix.

procedure dp_pmatrix (matrix, coeff, ndimen, rel_bright)

real	matrix[ndimen,ARB]		# the square matrix
real	coeff[ARB]			# coefficient array
int	ndimen				# the number of dimensions
real	rel_bright			# relative brightness

int	i, j

begin
	do j = 1, ndimen {
	    do i = 1, ndimen
		matrix[i,j] = matrix[i,j] + rel_bright * coeff[i] * coeff[j]
	}
end


# DP_PSFLUT -- Compute the lookup table.

procedure dp_psflut (dao, psflut, coeff, npsf, xzero, yzero, ncenter)

pointer	dao			# pointer to the daophot structure
real	psflut[npsf,npsf,3]	# the lookup table
real	coeff[ARB]		# coefficient array
int	npsf			# the size of the lookup table
real	xzero, yzero		# x and y positions
int	ncenter			# center of the psf

int	i, j, status
pointer	psf
real	xinter, yinter, dfdxc, dfdyc, value
real	psf_interp()

begin
	psf = DP_PSF(dao)

	do j = 1, npsf {
	    yinter = yzero + (j - ncenter) / 2.
	    do i = 1, npsf {
		xinter = xzero + (i - ncenter) / 2.
		value = psf_interp (Memr[DP_PLOOKUP(psf)], DP_SZLOOKUP(psf),
		    DP_SZLOOKUP(psf), 0.0, 0.0, NO, xinter, yinter, dfdxc,
		    dfdyc, status)
		psflut[i,j,1] = psflut[i,j,1] + value
		if (DP_VARPSF(dao) == YES) {
		    psflut[i,j,2] = psflut[i,j,2] + value * coeff[2]
		    psflut[i,j,3] = psflut[i,j,3] + value * coeff[3]
		}
	    }
	}		    
end
