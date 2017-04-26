# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include "wdes.h"
include	"crtpict.h"

# CRT_LINEAR_HGRAM -- Calculate two histograms of an image.  One histogram
# shows the distribution of intensities in the untransformed image; the other
# shows the distribution of greyscale values in the transformed image. This
# procedure assumes a linear transformation.

procedure crt_linear_hgram (im, gp, z1, z2, ztrans, inten_hgram,greys_hgram)

pointer	im		 	# Pointer to image
pointer	gp			# Graphics descriptor
real	z1, z2			# Range of intensities mapped
int	ztrans			# Type of transfer function - linear or unitary
int	inten_hgram[NBINS]	# Output array of intensity hgram values
int	greys_hgram[NBINS]	# Output array of greyscale hgram values

pointer buf
int	npix, nsig_bits, zrange, mask, min_val, max_val
long	v[IM_MAXDIM]
int	dz1, dz2, high_zi, low_zi
real	high_z, low_z
bool	ggetb()
pointer	imgnlr(), imgnli()
int	ggeti()
errchk 	im_minmax, ggeti, imgnli, imgnlr

begin
	# If z1 and z2 not in graphcap, set to some reasonable numbers for
	# plots to be generated.
	if (ggetb (gp, "z1") && ggetb (gp, "z2")) {
	    dz1 = ggeti (gp, "z1")
	    dz2 = ggeti (gp, "z2")
	} else {
	    dz1 = 0
	    dz2 = 255
	}

	# Calculate number of bits of depth in output device
	zrange = ggeti (gp, "zr")
	for (nsig_bits = 0; ; nsig_bits = nsig_bits + 1) {
	    zrange = zrange / 2
	    if (zrange == 0)
		break
	}
	mask = (2 ** (nsig_bits)) - 1

	call aclri (inten_hgram, NBINS)
	call aclri (greys_hgram, NBINS)
	call amovkl (long(1), v, IM_MAXDIM)

	# Read lines into buffer and accumulate histograms.
	npix = IM_LEN(im,1)

	if (ztrans == W_UNITARY) {
	    min_val = int (IM_MIN(im))
	    max_val = int (IM_MAX(im))
	    while (imgnli (im, buf, v) != EOF) {
		call ahgmi (Memi[buf], npix, inten_hgram, NBINS, min_val,
		    max_val)
		call aandki  (Memi[buf], mask, Memi[buf], npix)
		call ahgmi  (Memi[buf], npix, greys_hgram, NBINS, dz1, dz2)
	    }
	} else if (IM_PIXTYPE(im) == TY_SHORT) {
	    min_val = int (IM_MIN(im))
	    max_val = int (IM_MAX(im))
	    if (z2 > z1) {
		# Positive contrast
		high_zi = int (z2)
		low_zi  = int (z1)
	    } else {
		# Negative contrast
		high_zi = int (z1)
		low_zi  = int (z2)
	    }
	    while (imgnli (im, buf, v) != EOF) {
		call ahgmi (Memi[buf], npix, inten_hgram, NBINS, min_val, 
		    max_val)
		call ahgmi (Memi[buf], npix, greys_hgram, NBINS, low_zi,
		    high_zi)
	    }
	} else {
	    if (z2 > z1) {
		# Positive contrast
		high_z = z2
		low_z  = z1
	    } else {
		# Negative contrast
		high_z = z1
		low_z  = z2
	    }
	    while (imgnlr (im, buf, v) != EOF) {
	        call ahgmr (Memr[buf], npix, inten_hgram, NBINS, IM_MIN(im),
		    IM_MAX(im))
	        call ahgmr (Memr[buf], npix, greys_hgram, NBINS, low_z, high_z)
	    }
	} 
end


# CRT_USER_HGRAM -- Calculate two histograms of an image.  One histogram
# shows the distribution of intensities in the untransformed image; the other
# shows the distribution of greyscale values in the transformed image. This
# procedure does not assume a linear transformation, but rather uses a user
# specified look up table.

procedure crt_user_hgram (im, gp, z1, z2, lut, inten_hgram, greys_hgram)

pointer	im		 	# Pointer to image
pointer	gp			# Graphics descriptor
real	z1, z2			# Range of intensities mapped
short	lut[ARB]		# Look up table previously calculated
int	inten_hgram[NBINS]	# Output array of intensity hgram values
int	greys_hgram[NBINS]	# Output array of greyscale hgram values

pointer buf, ibuf, sp, rlut
short	min_val, max_val, short_min, short_max, dz1, dz2
int	npix
long	v[IM_MAXDIM]
short	high_zi, low_zi
real	high_z, low_z
pointer	imgnlr(), imgnls()
errchk 	im_minmax, imgnls, imgnlr

begin
	# Get max and min in look up table
	call alims (lut, SZ_BUF, dz1, dz2)

	call aclri (inten_hgram, NBINS)
	call aclri (greys_hgram, NBINS)
	call amovkl (long(1), v, IM_MAXDIM)

	# Read lines into buffer and accumulate histograms.
	npix = IM_LEN(im,1)

	if (IM_PIXTYPE(im) == TY_SHORT) {
	    min_val = short (IM_MIN(im))
	    max_val = short (IM_MAX(im))
	    short_min = short (STARTPT)
	    short_max = short (ENDPT)

	    if (z2 > z1) {
		# Positive contrast
		high_zi = short (z2)
		low_zi  = short (z1)
	    } else {
		# Negative contrast
		high_zi = short (z1)
		low_zi  = short (z2)
	    }

	    while (imgnls (im, buf, v) != EOF) {
		call ahgms (Mems[buf], npix, inten_hgram, NBINS, min_val, 
		    max_val)
		call amaps  (Mems[buf], Mems[buf], npix, low_zi, high_zi, 
		    short_min, short_max)
		call aluts  (Mems[buf], Mems[buf], npix, lut)
		call ahgms (Mems[buf], npix, greys_hgram, NBINS, dz1, dz2)
	    }
	} else {
	    if (z2 > z1) {
		# Positive contrast
		high_z = z2
		low_z  = z1
	    } else {
		# Negative contrast
		high_z = z1
		low_z  = z2
	    }

	    call smark (sp)
	    call salloc (ibuf, npix, TY_INT)
	    call salloc (rlut, SZ_BUF, TY_REAL)
	    call achtsr (lut, Memr[rlut], SZ_BUF)

	    while (imgnlr (im, buf, v) != EOF) {
	        call ahgmr (Memr[buf], npix, inten_hgram, NBINS, IM_MIN(im),
		    IM_MAX(im))

		call amapr (Memr[buf], Memr[buf], npix, z1, z2, STARTPT, ENDPT)
		call achtri (Memr[buf], Memi[ibuf], npix)
		call alutr (Memi[ibuf], Memr[buf], npix, Memr[rlut])
	        call ahgmr (Memr[buf], npix, greys_hgram, NBINS, real (dz1),
		    real (dz2))
	    }

	    call sfree (sp)
	} 
end
