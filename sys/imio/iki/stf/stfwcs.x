# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<imhdr.h>
include	<imio.h>
include	"stf.h"

.help stfwcs
.nf ---------------------------------------------------------------------------
STFWCS -- This is a minimal package to support operations upon the world
coordinate systems used int the STF image format.  Our principal task is
to read, write, and transform the WCS, e.g., to copy a WCS when a new-copy
image is made, or to transform the WCS when an image section is copied.

	      stf_copywcs (o_im, n_im)		# copy wcs to new image
        wcs = stf_loadwcs (im)			# load wcs into descriptor
	      stf_savewcs (wcs, im)		# save wcs in image
              stf_freewcs (wcs)			# free descriptor
	     stf_pixtowcs (wcs, pv, wv)		# coordinate xform, pix->wcs

The WCS used herein applies the following type transformation to go from
pixel coords to world coords (two dimensional case shown):

	x' = CD1_1 * (x - x0) + CD1_2 (y - y0)
	y' = CD2_1 * (x - x0) + CD2_2 (y - y0)

.endhelp ----------------------------------------------------------------------

# STF WCS descriptor.

define	LEN_SWCDES	135
define	SZ_SWCCTYPE	8
define	SZ_PNAME	8

define	SWC_NAXIS	Memi[$1]		# number of axes
define	SWC_CRPIX	Memr[$1+($2-1)+10]	# reference pixel
define	SWC_CRVAL	Memd[P2D($1+20)+$2-1]	# reference pixel coord value
define	SWC_CDVECTOR	Memr[$1+($2-1)*7+40]	# transformation matrix
define	SWC_CDMATRIX	Memr[$1+($3-1)*7+($2-1)+40]
define	SWC_CTYPE	Memc[P2C($1+($2-1)*9+90)]	# coord type strings


# STF_COPYWCS -- Copy the WCS from one image to another.  If the input image
# does not have WCS information, quit.

procedure stf_copywcs (o_im, n_im)

pointer	o_im			# old (input) image
pointer	n_im			# new (output) image

pointer	wcs
pointer	stf_loadwcs()
errchk	stf_loadwcs
int	imaccf()

begin
	# Be sure the WCS information is present in the old image as some
	# STF format images do not have WCS parameters available.

	if (imaccf (o_im, "CRPIX1") == YES && 
            imaccf (o_im, "CRVAL1") == YES &&
            imaccf (o_im, "CD1_1") == YES) {

	    wcs = stf_loadwcs (o_im)
	    call stf_savewcs (wcs, n_im)
	    call stf_freewcs (wcs)
	}
end


# STF_LOADWCS -- Load the WCS from an image header into a new WCS descriptor,
# returning a pointer to the WCS descriptor to the calling program.  
# If the input image has a WCS and was opened with an image section, apply
# the section transformation to produce the WCS for the section seen by the
# program.

pointer procedure stf_loadwcs (im)

pointer	im			# image descriptor

bool	found_wcs
pointer	o_wcs, n_wcs
char	pname[SZ_PNAME]
int	v_keep[IM_MAXDIM], nldim, npdim, i, j, ii, ij

real	pv[IM_MAXDIM]
double	wv[IM_MAXDIM]
long	o_pv[IM_MAXDIM], n_pv[IM_MAXDIM]
real	v_offset[IM_MAXDIM], v_scale[IM_MAXDIM]

real	imgetr()
double	imgetd()
errchk	imgetr, imgetd, imgstr

string  wcserr "CRPIX, CRVAL, or CD for WCS not found; used PIXEL WCS (%s)\n"
string  ctypeerr "CTYPE not found; used WCS with CTYPE = UNKNOWN (%s)\n"

begin
	call calloc (o_wcs, LEN_SWCDES, TY_STRUCT)
	call calloc (n_wcs, LEN_SWCDES, TY_STRUCT)

	npdim = IM_NPHYSDIM(im)
	nldim = IM_NDIM(im)
	SWC_NAXIS(o_wcs) = npdim
	SWC_NAXIS(n_wcs) = nldim

	# Attempt to read the WCS information from the old image header into
	# the WCS descriptor.  If any of the required parameters turn up
	# missing, bag it and manufacture a pixel WCS.

	iferr {
	    do j = 1, npdim {
		call sprintf (pname, SZ_PNAME, "CRPIX%d"); call pargi (j)
		SWC_CRPIX(o_wcs,j) = imgetr (im, pname)
		call sprintf (pname, SZ_PNAME, "CRVAL%d"); call pargi (j)
		SWC_CRVAL(o_wcs,j) = imgetd (im, pname)

		do i = 1, nldim {
		    call sprintf (pname, SZ_PNAME, "CD%d_%d")
			call pargi (i); call pargi (j)
		    SWC_CDMATRIX(o_wcs,i,j) = imgetr (im, pname)
		}

		call sprintf (pname, SZ_PNAME, "CTYPE%d"); call pargi (j)
		iferr (call imgstr (im, pname, SWC_CTYPE(o_wcs,j), 
			SZ_SWCCTYPE)) {
		    call eprintf (ctypeerr)
		    call pargstr (IM_NAME(im))
		    call strcpy ("UNKNOWN", SWC_CTYPE(n_wcs, j), SZ_SWCCTYPE)
		}

	    }

	    found_wcs = true

	} then {
	    # Could not find some WCS information in the image header.  
	    # Manufacture a pixel WCS for the image.

	    call eprintf (wcserr)
	    call pargstr (IM_NAME(im))

	    do j = 1, nldim {
		SWC_CRPIX(n_wcs,j) = 1.
		SWC_CRVAL(n_wcs,j) = 1.
		call strcpy ("PIXEL", SWC_CTYPE(n_wcs,j), SZ_SWCCTYPE)

		do i = 1, nldim {
		    if (i == j)
			SWC_CDMATRIX(n_wcs,i,j) = 1.0
		    else
			SWC_CDMATRIX(n_wcs,i,j) = 0.0
		}
	    }

	    found_wcs = false
	}

	# If we found a WCS in the old image header and an image section
	# transformation is in effect on the input image, transform the
	# WCS accordingly.  This is not necessary if no WCS was found as
	# the manufactured WCS will reflect the pixel coordinates of the
	# image section.  The section transformation is applied by setting
	# up a transformation matrix for the image section and performing a
	# the original WCS matrix by the section transformation matrix,
	# to get the new WCS matrix, then adding the section offset to the
	# CRPIX terms.
	#
        #        [x']   [CD1_1  CD1_2]   [(x-x0)]
        #             =                *        
        #        [y']   [CD2_1  CD2_2]   [(y-y0)]
	#
	# Since image sections to not permit rotations the section
	# transformation matrix will be a diagonal matrix, with the magnitude
	# of the diagonal terms being the subsampling factor and a negative
	# sign indicating an axis flip.  Multiplication by a diagonal matrix
	# is equivalent to multiplying each column of the full WCS matrix by
	# the corresponding diagonal term.  If the section has fewer dimensions
	# than the original image, i.e., if axis N is a constant, then we must
	# eliminate the Nth row and column from the input WCS matrix.

	if (found_wcs && IM_SECTUSED(im) == YES) {
	    # Determine which axes to preserve in the new coordinate system.
	    do i = 1, npdim
		v_keep[i] = NO
	    do i = 1, nldim
		v_keep[IM_VMAP(im,i)] = YES

	    # Determine the parameters of a linear, nonrotated transformation.
	    do i = 1, npdim {
		v_offset[i] = IM_VOFF(im,i)
		v_scale[i]  = IM_VSTEP(im,i)
	    }

	    # Set up the reference pixel vector ([1,1,1...] currently) and
	    # compute the corresponding world vector.

	    call amovkl (long(1), n_pv, nldim)
	    call imaplv (im, n_pv, o_pv, nldim)
	    call achtlr (o_pv, pv, npdim)
	    call stf_pixtowcs (o_wcs, pv, wv)

	    # Generate the new WCS matrix and other transformation parameters
	    # (crpix, crval, and crtype).

	    ij = 1
	    do j = 1, npdim
		if (v_keep[j] == YES) {
		    ii = 1
		    do i = 1, npdim
			if (v_keep[i] == YES) {
			    SWC_CDMATRIX(n_wcs,ii,ij) =
				SWC_CDMATRIX(o_wcs,i,j) * v_scale[j]
			    ii = ii + 1
			}

		    SWC_CRPIX(n_wcs,ij) = n_pv[ij]
		    SWC_CRVAL(n_wcs,ij) = wv[j]
		    call strcpy (SWC_CTYPE(o_wcs,j), SWC_CTYPE(n_wcs,ij),
			SZ_SWCCTYPE)
		    ij = ij + 1
		}

	} else if (found_wcs) {
	    # The old image had a WCS but no section was specified.  Simply
	    # copy the old WCS to the new (the above code could have been
	    # used to do this as well since a unitary transformation would
	    # be set up in the absence of an image section).

	    call mfree (n_wcs, TY_STRUCT)
	    return (o_wcs)
	}

	call mfree (o_wcs, TY_STRUCT)
	return (n_wcs)
end


# STF_SAVEWCS -- Update the WCS parameters in the image header.

procedure stf_savewcs (wcs, im)

pointer	wcs			# WCS descriptor
pointer	im			# image in which WCS is to be updated

int	naxis, i, j
char	pname[SZ_PNAME]
errchk	imputr, impstr

begin
	naxis = IM_NDIM(im)

	iferr {
	    do j = 1, naxis {
		call sprintf (pname, SZ_PNAME, "CRPIX%d"); call pargi (j)
		call imputr (im, pname, SWC_CRPIX(wcs,j))
		call sprintf (pname, SZ_PNAME, "CRVAL%d"); call pargi (j)
		call imputd (im, pname, SWC_CRVAL(wcs,j))
		call sprintf (pname, SZ_PNAME, "CTYPE%d"); call pargi (j)
		call impstr (im, pname, SWC_CTYPE(wcs,j))

		do i = 1, naxis {
		    call sprintf (pname, SZ_PNAME, "CD%d_%d")
			call pargi (i); call pargi (j)
		    call imputr (im, pname, SWC_CDMATRIX(wcs,i,j))
		}
	    }
	} then
	    call erract (EA_WARN)
end


# STF_PIXTOWCS -- Evaluate the world coordinates of a vector input in pixel
# coordinates.

procedure stf_pixtowcs (wcs, pv, wv)

pointer	wcs			# WCS descriptor
real	pv[ARB]			# point in pixel space (input)
double	wv[ARB]			# world coordinates of same point (output)

int	naxis, i, j
real	vv[IM_MAXDIM]

begin
	naxis = SWC_NAXIS(wcs)
	do i = 1, naxis {
	    vv[i] = pv[i] - SWC_CRPIX(wcs,i)
	    wv[i] = SWC_CRVAL(wcs,i)
	}
	do j = 1, naxis
	    do i = 1, naxis
		wv[j] = wv[j] + SWC_CDMATRIX(wcs,i,j) * vv[j]
end


# STF_FREEWCS -- Free the WCS descriptor

procedure stf_freewcs (wcs)

pointer	wcs			# WCS descriptor

begin
	call mfree (wcs, TY_STRUCT)
end
