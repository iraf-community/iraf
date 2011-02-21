# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include <imset.h>
include <imhdr.h>
include	<error.h>
include	<pmset.h>
include	"display.h"
include	"gwindow.h"

# DISPLAY - Display an image.  The specified image section is mapped into
# the specified section of an image display frame.  The mapping involves
# a linear transformation in X and Y and a linear or logarithmic transformation
# in Z (greyscale).  Images of all pixel datatypes are supported, and there
# no upper limit on the size of an image.  The display device is interfaced
# to FIO as a file and is accessed herein via IMIO as just another imagefile.
# The physical characteristics of the display (i.e., X, Y, and Z resolution)
# are taken from the image header.  The display frame buffer is the pixel
# storage "file".

# This is a version of the standard display that allows the overlay mask
# to be manipuated in memory prior to displaying.

procedure t_acedisplay()

char	image[SZ_FNAME]		# Image to display
int	frame			# Display frame
int	erase			# Erase frame?

int	i
pointer	sp, wdes, im, ds, ovrly

bool	clgetb()
int	clgeti(), btoi()
pointer	immap(), imd_mapframe1(), overlay()
errchk	immap, imd_mapframe1
errchk	ds_getparams, ds_setwcs, ds_load_display, ds_erase_border

begin
	call smark (sp)
	call salloc (wdes, LEN_WDES, TY_STRUCT)
	call aclri (Memi[wdes], LEN_WDES)

	# Open input imagefile.
	call clgstr ("image", image, SZ_FNAME)
	im = immap (image, READ_ONLY, 0)
	if (IM_NDIM(im) <= 0)
	    call error (1, "image has no pixels")

	# Open display device as an image.
	frame = clgeti ("frame")
	erase = btoi (clgetb ("erase"))
	if (erase == YES)
	    ds = imd_mapframe1 (frame, WRITE_ONLY,
		btoi (clgetb ("select_frame")), erase)
	else
	    ds = imd_mapframe1 (frame, READ_WRITE,
		btoi (clgetb ("select_frame")), erase)

	# Get display parameters and set up transformation.
	call ds_getparams (im, ds, wdes)

	# Compute and output the screen to image pixel WCS.
	call ds_setwcs (im, ds, wdes, image, frame)

	# Setup the overlay.
	ovrly = overlay (W_OVRLY(wdes), im)

	# Display the image and zero the border if necessary.
	call ods_load_display (im, ds, wdes, ovrly)
	if (!clgetb ("erase") && clgetb ("border_erase"))
	    call ds_erase_border (im, ds, wdes)

	# Free storage.
	call maskcolor_free (W_OCOLORS(wdes))
	call maskcolor_free (W_BPCOLORS(wdes))
	do i = 0, W_MAXWC
	    if (W_UPTR(W_WC(wdes,i)) != NULL)
		call ds_ulutfree (W_UPTR(W_WC(wdes,i)))
	if (ovrly != NULL)
	    call imunmap (ovrly)
	call imunmap (ds)
	call imunmap (im)

	call sfree (sp)
end


# DS_LOAD_DISPLAY -- Map an image into the display window.  In general this
#   involves independent linear transformations in the X, Y, and Z (greyscale)
#   dimensions.  If a spatial dimension is larger than the display window then
#   the image is block averaged.  If a spatial dimension or a block averaged
#   dimension is smaller than the display window then linear interpolation is
#   used to expand the image.  Both the input image and the output device appear
#   to us as images, accessed via IMIO.  All spatial scaling is 
#   handled by the "scaled input" package, i.e., SIGM2[SR].  Our task is to
#   get lines from the scaled input image, transform the greyscale if necessary,
#   and write the lines to the output device.

# This version passes the overlay mask pointer rather than mapping it.
# Otherwise this is unchanged from the standard version.

procedure ods_load_display (im, ds, wdes, ovrly)

pointer	im			# input image
pointer	ds			# output image
pointer	wdes			# graphics window descriptor
pointer	ovrly			# overlay pointer

real	z1, z2, dz1, dz2, px1, px2, py1, py2
int	i, order, zt, wx1, wx2, wy1, wy2, wy, nx, ny, xblk, yblk
pointer	wdwin, wipix, wdpix, bpm, pm, uptr
pointer	in, out, si, si_ovrly, si_bpovrly, ocolors, bpcolors, rtemp
bool	unitary_greyscale_transformation
short	lut1, lut2, dz1_s, dz2_s, z1_s, z2_s

bool	fp_equalr()
int	imstati()
real	if_elogr()
pointer	ds_pmmap(), imps2s(), imps2r(), sigm2s(), sigm2r(), sigm2_setup()
errchk	ds_pmmap, imps2s, imps2r, sigm2s, sigm2r, sigm2_setup

extern  if_elogr

begin
	wdwin = W_WC(wdes,W_DWIN)
	wipix = W_WC(wdes,W_IPIX)
	wdpix = W_WC(wdes,W_DPIX)

	# Set image and display pixels.
	px1 = nint (W_XS(wipix))
	px2 = nint (W_XE(wipix))
	py1 = nint (W_YS(wipix))
	py2 = nint (W_YE(wipix))
	wx1 = nint (W_XS(wdpix))
	wx2 = nint (W_XE(wdpix))
	wy1 = nint (W_YS(wdpix))
	wy2 = nint (W_YE(wdpix))

	z1 = W_ZS(wdwin)
	z2 = W_ZE(wdwin)
	zt = W_ZT(wdwin)
	uptr = W_UPTR(wdwin)
	order = max (W_XT(wdwin), W_YT(wdwin))

	# Setup scaled input and masks.
	si = NULL
	si_ovrly = NULL
	si_bpovrly = NULL
	nx = wx2 - wx1 + 1
	ny = wy2 - wy1 + 1
	xblk = INDEFI
	yblk = INDEFI

	ocolors = W_OCOLORS(wdes)
#	iferr (ovrly = ds_pmmap (W_OVRLY(wdes), im)) {
#	    call erract (EA_WARN)
#	    ovrly = NULL
#	}
	if (ovrly != NULL) {
	    xblk = INDEFI
	    yblk = INDEFI
	    si_ovrly = sigm2_setup (ovrly, NULL, px1,px2,nx,xblk,
		py1,py2,ny,yblk, -1)
	}

	bpcolors = W_BPCOLORS(wdes)
	switch (W_BPDISP(wdes)) {
	case BPDNONE:
	    si = sigm2_setup (im, NULL, px1,px2,nx,xblk, py1,py2,ny,yblk, order)
	case BPDOVRLY:
	    si = sigm2_setup (im, NULL, px1,px2,nx,xblk, py1,py2,ny,yblk, order)
	    iferr (bpm = ds_pmmap (W_BPM(wdes), im))
		bpm = NULL
	    if (bpm != NULL)
		si_bpovrly = sigm2_setup (bpm, NULL, px1,px2,nx,xblk,
		    py1,py2,ny,yblk, -1)
	case BPDINTERP:
	    iferr (bpm = ds_pmmap (W_BPM(wdes), im))
		bpm = NULL
	    if (bpm != NULL)
		pm = imstati (bpm, IM_PMDES)
	    else
		pm = NULL
	    si = sigm2_setup (im, pm, px1,px2,nx,xblk, py1,py2,ny,yblk, order)
	}

	# The device IM_MIN and IM_MAX parameters define the acceptable range
	# of greyscale values for the output device (e.g., 0-255 for most 8-bit
	# display devices).  Values Z1 and Z2 are mapped linearly or
	# logarithmically into IM_MIN and IM_MAX.

	dz1 = IM_MIN(ds)
	dz2 = IM_MAX(ds)
	if (fp_equalr (z1, z2)) {
	    z1 = z1 - 1
	    z2 = z2 + 1
	}

	# If the user specifies the transfer function, verify that the
	# intensity and greyscale are in range.

	if (zt == W_USER) {
	    call alims (Mems[uptr], U_MAXPTS, lut1, lut2)
	    dz1_s = short (dz1)
	    dz2_s = short (dz2)
	    if (lut2 < dz1_s || lut1 > dz2_s)
		call eprintf ("User specified greyscales out of range\n")
	    if (z2 < IM_MIN(im) || z1 > IM_MAX(im))
		call eprintf ("User specified intensities out of range\n")
	}

	# Type short pixels are treated as a special case to minimize vector
	# operations for such images (which are common).  If the image pixels
	# are either short or real then only the ALTR (greyscale transformation)
	# vector operation is required.  The ALTR operator linearly maps
	# greylevels in the range Z1:Z2 to DZ1:DZ2, and does a floor ceiling
	# of DZ1:DZ2 on all pixels outside the range.  If unity mapping is
	# employed the data is simply copied, i.e., floor ceiling constraints
	# are not applied.  This is very fast and will produce a contoured
	# image on the display which will be adequate for some applications.

	if (zt == W_UNITARY) {
	    unitary_greyscale_transformation = true
	} else if (zt == W_LINEAR) {
	    unitary_greyscale_transformation =
		(fp_equalr(z1,dz1) && fp_equalr(z2,dz2))
	} else
	    unitary_greyscale_transformation = false

	if (IM_PIXTYPE(im) == TY_SHORT && zt != W_LOG) {
	    z1_s = z1;  z2_s = z2
	    if (z1_s == z2_s) {
		z1_s = z1_s - 1
		z2_s = z2_s + 1
	    }

	    for (wy=wy1;  wy <= wy2;  wy=wy+1) {
		in  = sigm2s (si, wy - wy1 + 1)
		out = imps2s (ds, wx1, wx2, wy, wy)

		if (unitary_greyscale_transformation) {
		    call amovs (Mems[in], Mems[out], nx)
		} else if (zt == W_USER) {
		    dz1_s = U_Z1; dz2_s = U_Z2
		    call amaps (Mems[in],Mems[out],nx, z1_s,z2_s, dz1_s,dz2_s)
		    call aluts (Mems[out], Mems[out], nx, Mems[uptr])
		} else {
		    dz1_s = dz1; dz2_s = dz2
		    call amaps (Mems[in],Mems[out],nx, z1_s,z2_s, dz1_s,dz2_s)
		}

		if (si_ovrly != NULL) {
		    in = sigm2s (si_ovrly, wy - wy1 + 1)
		    do i = 0, nx-1 {
			if (Mems[in+i] != 0)
			    call mcolors (ocolors, int(Mems[in+i]),
				Mems[out+i])
		    }
		}
		if (si_bpovrly != NULL) {
		    in = sigm2s (si_bpovrly, wy - wy1 + 1)
		    do i = 0, nx-1 {
			if (Mems[in+i] != 0)
			    call mcolors (bpcolors, int(Mems[in+i]),
				Mems[out+i])
		    }
		}
	    }

	} else if (zt == W_USER) {
	    call salloc (rtemp, nx, TY_REAL)

	    for (wy=wy1;  wy <= wy2;  wy=wy+1) {
		in  = sigm2r (si, wy - wy1 + 1)
		out = imps2s (ds, wx1, wx2, wy, wy)

		call amapr (Memr[in], Memr[rtemp], nx, z1, z2, 
		    real(U_Z1), real(U_Z2))
		call achtrs (Memr[rtemp], Mems[out], nx)
		call aluts (Mems[out], Mems[out], nx, Mems[uptr])

		if (si_ovrly != NULL) {
		    in = sigm2s (si_ovrly, wy - wy1 + 1)
		    do i = 0, nx-1 {
			if (Mems[in+i] != 0)
			    call mcolors (ocolors, int(Mems[in+i]),
				Mems[out+i])
		    }
		}
		if (si_bpovrly != NULL) {
		    in = sigm2s (si_bpovrly, wy - wy1 + 1)
		    do i = 0, nx-1 {
			if (Mems[in+i] != 0)
			    call mcolors (bpcolors, int(Mems[in+i]),
				Mems[out+i])
		    }
		}
	    }

	} else {
	    for (wy=wy1;  wy <= wy2;  wy=wy+1) {
		in  = sigm2r (si, wy - wy1 + 1)
		out = imps2r (ds, wx1, wx2, wy, wy)

		if (unitary_greyscale_transformation)  {
		    call amovr (Memr[in], Memr[out], nx)
		} else if (zt == W_LOG) {
		    call amapr (Memr[in], Memr[out], nx,
			z1, z2, 1.0, 10.0 ** MAXLOG)
		    call alogr (Memr[out], Memr[out], nx, if_elogr)
		    call amapr (Memr[out], Memr[out], nx,
			0.0, real(MAXLOG), dz1, dz2)
		} else
		    call amapr (Memr[in], Memr[out], nx, z1, z2, dz1, dz2)

		if (si_ovrly != NULL) {
		    in = sigm2s (si_ovrly, wy - wy1 + 1)
		    do i = 0, nx-1 {
			if (Mems[in+i] != 0)
			    call mcolorr (ocolors, int(Mems[in+i]),
				Memr[out+i])
		    }
		}
		if (si_bpovrly != NULL) {
		    in = sigm2s (si_bpovrly, wy - wy1 + 1)
		    do i = 0, nx-1 {
			if (Mems[in+i] != 0)
			    call mcolorr (bpcolors, int(Mems[in+i]),
				Memr[out+i])
		    }
		}
	    }
	}

	call sigm2_free (si)
	if (si_ovrly != NULL)
	    call sigm2_free (si_ovrly)
	if (si_bpovrly != NULL)
	    call sigm2_free (si_bpovrly)
#	if (ovrly != NULL)
#	    call imunmap (ovrly)
	if (bpm != NULL)
	    call imunmap (bpm)
end


# The ds_pmmap routines needed to be modified for 27 bit masks.

include	<mach.h>
include	<ctype.h>
include	<error.h>
include	<imhdr.h>
include	<imset.h>
include	<pmset.h>
include	<syserr.h>


# DS_PMMAP -- Open a pixel mask READ_ONLY.
#
# Open the pixel mask.  If a regular image is specified convert it to
# a pixel mask.  Match the mask to the reference image based on the
# physical coordinates.  A null filename is allowed and returns NULL.

pointer procedure ods_pmmap (pmname, refim)

char	pmname[ARB]		#I Pixel mask name
pointer	refim			#I Reference image pointer

pointer	im
char	fname[SZ_FNAME]
int	nowhite(), errcode()
bool	streq()
pointer	im_pmmap(), ods_immap()
errchk	ods_immap, ods_match

begin
	if (nowhite (pmname, fname, SZ_FNAME) == 0)
	    return (NULL)
	if (streq (fname, "EMPTY"))
	    return (NULL)
	if (streq (fname, "BPM")) {
	    iferr (call imgstr (refim, "BPM", fname, SZ_FNAME))
		return (NULL)
	}

	iferr (im = im_pmmap (fname, READ_ONLY, NULL)) {
	    switch (errcode()) {
	    case SYS_FOPNNEXFIL, SYS_PLBADSAVEF:
		im = ods_immap (fname, refim)
	    default:
		call erract (EA_ERROR)
	    }
	}

	iferr (call ods_match (im, refim))
	    call erract (EA_WARN)

	return (im)
end


# DS_PMIMMAP -- Open a pixel mask from a non-pixel list image.
# Return error if the image cannot be opened.

pointer procedure ods_immap (pmname, refim)

char	pmname[ARB]		#I Image name
pointer	refim			#I Reference image pointer

short	val
int	i, ndim, npix
pointer	sp, v1, v2, im_in, im_out, pm, mw, data

int	imgnli()
pointer immap(), pm_newmask(), im_pmmapo(), imgl1i(), mw_openim()
errchk	immap, mw_openim

begin
	call smark (sp)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)

	call amovkl (long(1), Meml[v1], IM_MAXDIM)
	call amovkl (long(1), Meml[v2], IM_MAXDIM)

	im_in = immap (pmname, READ_ONLY, 0)
	pm = pm_newmask (im_in, 16)

	ndim = IM_NDIM(im_in)
	npix = IM_LEN(im_in,1)

	while (imgnli (im_in, data, Meml[v1]) != EOF) {
	    do i = 0, npix-1 {
		val = Memi[data+i]
		if (val < 0)
		   Memi[data+i] = 0
	    }
	    call pmplpi (pm, Meml[v2], Memi[data], 0, npix, PIX_SRC)
	    call amovl (Meml[v1], Meml[v2], ndim)
	}

	im_out = im_pmmapo (pm, im_in)
	data = imgl1i (im_out)		# Force I/O to set header
	mw = mw_openim (im_in)		# Set WCS
	call mw_saveim (mw, im_out)
	call mw_close (mw)

	call imunmap (im_in)
	call sfree (sp)
	return (im_out)
end


# DS_MATCH -- Set the pixel mask to match the reference image.
# This matches sizes and physical coordinates and allows the
# original mask to be smaller or larger than the reference image.
# Subsequent use of the pixel mask can then work in the logical
# coordinates of the reference image.  A null input returns a null output.

procedure ods_match (im, refim)

pointer	im			#U Pixel mask image pointer
pointer	refim			#I Reference image pointer

int	i, j, k, nc, nl, ncpm, nlpm, c1, c2, l1, l2, nref, npm
int	steptype, xoffset, xstep, yoffset, ystep
double	x1, x2, y1, y2
long	vold[IM_MAXDIM], vnew[IM_MAXDIM]
pointer	mwref, mwpm, ctref, ctpm, pm, pmnew, imnew, bufref, bufpm

int	imstati()
pointer	pm_open(), mw_openim(), im_pmmapo(), imgl1i(), mw_sctran()
bool	pm_empty(), pm_linenotempty()
errchk	pm_open, mw_openim

begin
	if (im == NULL)
	    return

	# Set sizes.
	pm = imstati (im, IM_PMDES)
	nc = IM_LEN(refim,1)
	nl = IM_LEN(refim,2)
	ncpm = IM_LEN(im,1)
	nlpm = IM_LEN(im,2)

	# Check if the two are the same logical size and the mask is empty.
	if (nc == ncpm && nl == nlpm && pm_empty (pm))
	    return

	# Check coordinate transformations.
	mwref = mw_openim (refim)
	mwpm = mw_openim (im)

	steptype = 1
	ctref = mw_sctran (mwref, "logical", "physical", 3)
	ctpm = mw_sctran (mwpm, "physical", "logical", 3)
	call mw_c2trand (ctref, 1D0, 1D0, x1, y1) 
	call mw_c2trand (ctpm, x1, y1, x1, y1) 
	call mw_c2trand (ctref, 2D0, 1D0, x2, y2) 
	call mw_c2trand (ctpm, x2, y2, x2, y2) 
	if (abs(x2-x1) < 1.) {
	    steptype = 2
	    call mw_ctfree (ctref)
	    call mw_ctfree (ctpm)
	    ctref = mw_sctran (mwref, "physical", "logical", 3)
	    ctpm = mw_sctran (mwpm, "logical", "physical", 3)
	    call mw_c2trand (ctpm, 1D0, 1D0, x1, y1) 
	    call mw_c2trand (ctref, x1, y1, x1, y1) 
	    call mw_c2trand (ctpm, 2D0, 1D0, x2, y2) 
	    call mw_c2trand (ctref, x2, y2, x2, y2) 
	}
	x2 = x2 - x1
	if (abs(y1-y2) > 10*EPSILONR)
	    call error (0, "Image and mask have a relative rotation")
	if (abs(x1-nint(x1)) > 10*EPSILONR  &&
	    abs(x1-nint(x1))-0.5 > 10*EPSILONR)
	    call error (0, "Image and mask have non-integer relative offsets")
	if (abs(x2-nint(x2)) > 10*EPSILONR)
	    call error (0, "Image and mask have non-integer relative steps")
	xoffset = nint (x1 - 1D0)
	xstep = nint (x2)

	if (steptype == 1) {
	    call mw_c2trand (ctref, 1D0, 1D0, x1, y1) 
	    call mw_c2trand (ctpm, x1, y1, x1, y1) 
	    call mw_c2trand (ctref, 1D0, 2D0, x2, y2) 
	    call mw_c2trand (ctpm, x2, y2, x2, y2) 
	} else {
	    call mw_c2trand (ctpm, 1D0, 1D0, x1, y1) 
	    call mw_c2trand (ctref, x1, y1, x1, y1) 
	    call mw_c2trand (ctpm, 1D0, 2D0, x2, y2) 
	    call mw_c2trand (ctref, x2, y2, x2, y2) 
	}
	y2 = y2 - y1
	if (abs(x1-x2) > 10*EPSILONR)
	    call error (0, "Image and mask have a relative rotation")
	if (abs(y1-nint(y1)) > 10*EPSILONR  &&
	    abs(y1-nint(y1))-0.5 > 10*EPSILONR)
	    call error (0, "Image and mask have non-integer relative offsets")
	if (abs(y2-nint(y2)) > 10*EPSILONR)
	    call error (0, "Image and mask have non-integer relative steps")
	yoffset = nint (y1 - 1D0)
	ystep = nint (y2)

	call mw_ctfree (ctref)
	call mw_ctfree (ctpm)
	call mw_close (mwref)
	call mw_close (mwpm)

	# Check if the two have the same coordinate system.
	if (nc==ncpm && nl==nlpm && xoffset==0 && yoffset==0 && xstep==ystep)
	    return

	# Create a new pixel mask of the required size and offset.
	pmnew = pm_open (NULL)
	call pm_ssize (pmnew, 2, IM_LEN(refim,1), 27)
	imnew = im_pmmapo (pmnew, NULL)
	bufref = imgl1i (imnew)

	if (steptype == 1) {
	    c1 = 1 + xoffset + max (0, (xstep - 1 - xoffset) / xstep) * xstep
	    c2 = 1 + xoffset + min (nc-1, (ncpm - 1 - xoffset) / xstep) * xstep
	    l1 = 1 + yoffset + max (0, (ystep - 1 - yoffset) / ystep) * ystep
	    l2 = 1 + yoffset + min (nl-1, (nlpm - 1 - yoffset) / ystep) * ystep
	    npm = c2 - c1 + 1
	    nref = npm / xstep
	    if (nref > 0) {
		call malloc (bufpm, npm, TY_INT)
		call malloc (bufref, nref, TY_INT)
		call amovkl (long(1), vold, IM_MAXDIM)
		call amovkl (long(1), vnew, IM_MAXDIM)
		vold[1] = c1
		vnew[1] = c1 - xoffset
		do i = l1, l2, ystep {
		    vold[2] = i
		    if (!pm_linenotempty (pm, vold))
			next
		    call pmglpi (pm, vold, Memi[bufpm], 0, npm, 0)
		    vnew[2] = l1 - yoffset + (i - l1) / ystep
		    j = 0
		    do k = 0, npm-1, xstep {
			Memi[bufref+j] = Memi[bufpm+k]
			j = j + 1
		    }
		    call pmplpi (pmnew, vnew, Memi[bufref], 0, nref, PIX_SRC)
		}
	    }
	} else {
	    c1 = max (1, 1 - xoffset)
	    c2 = min (ncpm, nc / xstep - xoffset)
	    l1 = max (1, 1 - yoffset)
	    l2 = min (nlpm, nl / ystep - yoffset)
	    npm = c2 - c1 + 1
	    nref = npm * xstep
	    if (nref > 0) {
		call malloc (bufpm, npm, TY_INT)
		call malloc (bufref, nref, TY_INT)
		call amovkl (long(1), vold, IM_MAXDIM)
		call amovkl (long(1), vnew, IM_MAXDIM)
		vold[1] = c1
		vnew[1] = c1 + xoffset
		do i = l1, l2 {
		    vold[2] = i
		    if (!pm_linenotempty (pm, vold))
			next
		    call pmglpi (pm, vold, Memi[bufpm], 0, npm, 0)
		    call aclri (Memi[bufref], nref)
		    do j = 0, npm-1 {
			k = j * xstep
			Memi[bufref+k] = Memi[bufpm+j]
		    }
		    vnew[2] = l1 + yoffset + (i - l1) * ystep
		    call pmplpi (pmnew, vnew, Memi[bufref], 0, nref, PIX_SRC)
		}
	    }
	    call mfree (bufpm, TY_INT)
	    call mfree (bufref, TY_INT)
	}

	# Update the IMIO descriptor.
	call imunmap (im)
	im = imnew
	call imseti (im, IM_PMDES, pmnew)
end


# IF_ELOG -- The error function for log10. Note that MAX_EXPONENT is
# currently an integer so it is converted to the appropriate data type
# before being returned.

real procedure if_elogr (x)

real   x                               # the input pixel value

begin
        return (real(-MAX_EXPONENT))
end
