include	<imhdr.h>
include <mwset.h>
include	<math.h>

define	SZ_PNAME	8

# stx_getcoord -- get coordinate parameters
# This procedure gets the coordinate parameters from an image.
# The parameter values are gotten via mwcs, and the lterm is factored
# into the wterm so that the parameters are relative to the actual image
# section that was opened rather than to the "original" image.
#
# Phil Hodge,  5-Jan-1993  Copied from fourier$lib/loadct.x.
# Phil Hodge,  2-Feb-1994  Errchk mwcs routines.

procedure stx_getcoord (im, crpix, crval, cd, maxdim, ctype, maxch)

pointer im			# i: pointer to imhdr struct for input image
double	crpix[ARB]		# o: reference pixel
double	crval[ARB]		# o: coordinates at reference pixel
double	cd[maxdim,maxdim]	# o: derivatives of l & m with respect to x & y
int	maxdim			# i: dimension of arrays (e.g. IM_MAXDIM)
char	ctype[maxch,maxdim]	# o: coord. type of each axis (e.g. "RA---TAN")
int	maxch			# i: size of ctype string (e.g. SZ_CTYPE)
#--
pointer mw

double	o_crval[IM_MAXDIM]		# world coordinates at reference pixel
double	o_crpix[IM_MAXDIM]		# not corrected for image section
double	o_cd[IM_MAXDIM,IM_MAXDIM]	# wterm only (not corr. for section)

double	n_crpix[IM_MAXDIM]		# corrected for image section
double	n_cd[IM_MAXDIM,IM_MAXDIM]	# CD matrix corrected for section

double	ltm[IM_MAXDIM,IM_MAXDIM]	# lterm matrix
double	i_ltm[IM_MAXDIM,IM_MAXDIM]	# inverse of ltm
double	ltv[IM_MAXDIM]			# lterm vector

int	ndim				# dimension of image
int	wcsdim				# dimension of mwcs coordinates
pointer mw_openim()
int	mw_stati()
errchk	mw_openim, mw_stati, mw_gwtermd, mw_gltermd, mw_invertd, mw_close,
	stx_extract

begin
	ndim = IM_NDIM(im)
	if (ndim > maxdim)
	    call error (1,
		"stx_getcoord:  dimension of image is larger than array size")

	mw = mw_openim (im)
	wcsdim = mw_stati (mw, MW_NPHYSDIM)		# get mwcs dimension

	# Get the wterm and the lterm.
	call mw_gwtermd (mw, o_crpix, o_crval, o_cd, wcsdim)
	call mw_gltermd (mw, ltm, ltv, wcsdim)

	# Convert the wterm to be the values relative to the current
	# image section.  (Comments & code copied from mwcs.)

	# Output CRPIX = R' =  (LTM * R + LTV).
	call mw_vmuld (ltm, o_crpix, n_crpix, wcsdim)
	call aaddd (ltv, n_crpix, n_crpix, wcsdim)

	# Output CD matrix = CD' =  (CD * inv(LTM)).
	call mw_invertd (ltm, i_ltm, wcsdim)
	call mw_mmuld (o_cd, i_ltm, n_cd, wcsdim)

	# Extract the coordinate parameters, and get ctype.
	call stx_extract (im, mw, n_crpix, o_crval, n_cd, wcsdim,
		crpix, crval, cd, maxdim, ctype, maxch)

	call mw_close (mw)

	# Check for invalid CD matrix.
	if (ndim == 1) {
	    if (cd[1,1] == 0.d0) {
		call eprintf ("warning:  pixel spacing = 0; reset to 1\n")
		cd[1,1] = 1.d0
	    }
	} else if (ndim == 2) {
	    if (cd[1,1] * cd[2,2] - cd[1,2] * cd[2,1] == 0.d0) {
		call eprintf (
		"warning:  CD matrix is singular; reset to identity matrix\n")
		cd[1,1] = 1.d0
		cd[2,1] = 0.d0
		cd[1,2] = 0.d0
		cd[2,2] = 1.d0
	    }
	}
end

# stx_extract -- extract coordinate parameters
# This routine is needed to take care of the situation where the dimension
# of the input image was reduced by taking an image section.  In that case,
# the coordinate information gotten using mwcs has the dimension of the
# original image, which results in two problems.  (1) We need to know which
# axis of the original image maps to which axis of the image that we've
# got.  (2) We have to dimension the CD matrix differently.  When MWCS
# puts values into a 2-D array it is dimensioned wcsdim X wcsdim, but we
# declared it maxdim X maxdim in the calling routine.  In this routine
# we declare the input CD matrix to be wcsdim X wcsdim, while the output
# CD matrix is maxdim X maxdim.

procedure stx_extract (im, mw, n_crpix, n_crval, n_cd, wcsdim,
		crpix, crval, cd, maxdim, ctype, maxch)

pointer im			# i: pointer to imhdr struct for input image
pointer mw			# i: mwcs pointer
double	n_crpix[wcsdim]		# i: crpix
double	n_crval[wcsdim]		# i: crval
double	n_cd[wcsdim,wcsdim]	# i: CD matrix
int	wcsdim			# i: dimension of wcs
double	crpix[maxdim]		# o: crpix extracted from n_crpix
double	crval[maxdim]		# o: crval extracted from n_crval
double	cd[maxdim,maxdim]	# o: CD matrix extracted from n_cd
int	maxdim			# i: dimension of arrays (e.g. IM_MAXDIM)
char	ctype[maxch,maxdim]	# o: coord. type of each axis (e.g. "RA---TAN")
int	maxch			# i: size of ctype string (e.g. SZ_CTYPE)
#--
char	keyword[SZ_PNAME]	# keyword for getting ctype
int	ndim			# actual dimension of image
int	axno[IM_MAXDIM]		# axis numbers
int	axval[IM_MAXDIM]	# ignored
int	ax[IM_MAXDIM]		# physical axis number for each logical axis
int	i, j
bool	ax_ok			# for checking that axis numbers were found
int	imaccf()
errchk	mw_gaxmap

begin
	ndim = IM_NDIM(im)

	# Get the axis mapping.
	call mw_gaxmap (mw, axno, axval, wcsdim)

	# Find the image axis numbers corresponding to the mwcs numbers.
	do i = 1, ndim				# initialize
	    ax[i] = 0
	do j = 1, wcsdim {
	    do i = 1, ndim {
		if (axno[j] == i) {
		    ax[i] = j
		    break
		}
	    }
	}

	# It's an error if any axis number was not found.
	ax_ok = true				# initial value
	do i = 1, ndim {
	    if (ax[i] < 1)
		ax_ok = false
	}
	if (!ax_ok) {
#	    call error (1, "stx_extract:  mwcs axis mapping is messed up")
# This is a temporary fix to prevent crashing on a vax.
	    do i = 1, ndim
		ax[i] = i
	}

	# Extract crpix, crval and the CD matrix.
	# Note that we transpose the CD matrix because of different
	# conventions regarding how a matrix is stored.
	do i = 1, ndim {
	    crpix[i] = n_crpix[ax[i]]
	    crval[i] = n_crval[ax[i]]
	    do j = 1, ndim
		cd[i,j] = n_cd[ax[j],ax[i]]	# transpose
	}

	# Get ctype.
	do i = 1, ndim {
	    call sprintf (keyword, SZ_PNAME, "ctype%d")
		call pargi (ax[i])			# physical axis number
	    if (imaccf (im, keyword) == YES)
		call imgstr (im, keyword, ctype[1,i], maxch)
	    else
		call strcpy ("PIXEL", ctype[1,i], maxch)
	}
end
