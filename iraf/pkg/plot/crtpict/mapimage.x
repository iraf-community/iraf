# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<mach.h>
include	<gset.h>
include	<error.h>
include	"wdes.h"
include	"crtpict.h"

# CRT_MAP_IMAGE -- Output a scaled image window to the device viewport.
# Spatial scaling is handled by the "scaled input" package, SIGL2[SR]; it is
# possible to scale the image window to a block averaged device viewport.
# Image intensities are converted to greyscale values and the input NDC
# coordinates are "tweaked" to make sure they represent integer device pixels.
# This tweaking also insures an integer replication factor between image
# pixels and device pixels.  Type short pixels are treated as a special
# case to minimize vector operations.

procedure crt_map_image (im, gp, px1,px2,py1,py2, ndc_xs,ndc_xe,ndc_ys,ndc_ye, 
        nx_output, ny_output, z1,z2,zt, cl)

pointer	im				# input image
pointer	gp				# graphics descriptor
real	px1,px2,py1,py2			# input section
real	ndc_xs,ndc_xe,ndc_ys,ndc_ye	# NDC of output section
int	nx_output, ny_output		# Number of output pixels.  Image pixels
					# are scaled to these dimensions.
real	z1,z2				# range of intensities to be mapped.
int	zt				# specified greyscale transform type
pointer	cl				# Pointer to crtpict structure

bool	unitary_greyscale_transformation
pointer	in, si, sline, rline, llut, sp
short	sz1, sz2, sdz1, sdz2, lut1, lut2
real	dz1, dz2, y1, y2, delta_y
int	ndev_cols, ndev_rows, nline, ny_device
int	xblk, yblk
bool	ggetb(), fp_equalr()
int	ggeti()
real	ggetr()
pointer	sigl2s(), sigl2r(), sigl2_setup()
errchk	sigl2s, sigl2r, sigl2_setup, ndc_tweak_ndc, ggeti, malloc, ggetr
errchk	ggetb, ggetr, gpcell, crt_ulut

begin
	call smark (sp)
	call salloc (sline, nx_output, TY_SHORT)
	if (IM_PIXTYPE(im) != TY_SHORT)
	    call salloc (rline, nx_output, TY_REAL)

	# Calculate and allocate heap space needed for an image row.
	ndev_cols = ggeti (gp, "xr")
	ndev_rows = ggeti (gp, "yr")
	ny_device  = ((ndc_ye * ndev_rows) - (ndc_ys * ndev_rows)) + 1

	# This sets up for the scaled image input 
	xblk = INDEFI
	yblk = INDEFI
	si = sigl2_setup (im, px1,px2,nx_output,xblk, py1,py2,ny_output,yblk)

	# If user has supplied look up table, it has to be dealt with at
	# this point.  Greyscale transform is coming up, and the transfer
	# function will be plotted at a later step.

	if (zt == W_USER) {
	    iferr (call crt_ulut (UFILE(cl), z1, z2, llut))
		call erract (EA_FATAL)
	    LUT(cl) = llut
	    call alims (Mems[llut], SZ_BUF, lut1, lut2)
	}

	# If device can't output greyscale information, return at this point.
	if (! ggetb (gp, "zr")) {
	    call eprintf ("Graphics device doesn't support greyscale output\n")
	    return
	}

	# Determine the device range for the greyscale transformation.
	if (ggetb (gp, "z1") && ggetb (gp, "z2")) {
	    dz1 = ggetr (gp, "z1")
	    dz2 = ggetr (gp, "z2")
	} else {
	    dz1 = 0.
	    dz2 = 255.
	}

	# And now a quick test to make sure user specified greyscale and
	# intensity ranges are reasonable.
	if (zt == W_USER) {
	    sdz1 = short (dz1)
	    sdz2 = short (dz2)
	    if (lut2 < sdz1 || lut1 > sdz2)
		call eprintf ("User specified greyscales out of range\n")
	    if (z2 < IM_MIN(im) || z1 > IM_MAX(im))
		call eprintf ("User specified intensities out of range\n")
	}

	if (zt == W_UNITARY)
	    unitary_greyscale_transformation = true
	else
	    unitary_greyscale_transformation =
		(fp_equalr (dz1,z1) && fp_equalr (dz2,z2)) || fp_equalr (z1,z2)

	# Calculate the delta_y, that is, the change in ndc coordinate
	# with each output row.  It has been assurred by tweak_ndc that
	# the ratio of device rows to output pixels is an integer.  

	delta_y = (real (ny_device) / ny_output) / real (ndev_rows)

	# For TY_SHORT pixels, pixel intensities are converted to greyscale
	# values, then output with gpcell.
	if (IM_PIXTYPE(im) == TY_SHORT) {
	    for (nline=1;  nline <= ny_output;  nline=nline+1) {
		in  = sigl2s (si, nline)
		if (unitary_greyscale_transformation)
		    call amovs (Mems[in], Mems[sline], nx_output)
		else if (zt == W_LINEAR) {
		    sz1  = short (z1)
		    sz2  = short (z2)
		    sdz1 = short (dz1)
		    sdz2 = short (dz2)
		    call amaps (Mems[in], Mems[sline], nx_output, sz1, sz2, 
			sdz1, sdz2)
		} else if (zt == W_USER) {
		    sz1  = short (z1)
		    sz2  = short (z2)
		    sdz1 = short (STARTPT)
		    sdz2 = short (ENDPT)
		    call amaps (Mems[in], Mems[sline], nx_output, sz1, sz2,
			sdz1, sdz2)
		    call aluts (Mems[sline], Mems[sline], nx_output, Mems[llut])
		}
		
		# Now put line out to greyscale device
		y1 = ndc_ys + (nline - 1) * delta_y
		y2 = ndc_ys + (nline * delta_y)

		call gpcell (gp, Mems[sline], nx_output, 1, ndc_xs, y1, ndc_xe,
		    y2)
	    }
	} else {
	    # Pixels are treated as TY_REAL; intensities are converted to
	    # greyscale values, then converted to TY_SHORT for gpcell output.
	    for (nline=1;  nline <= ny_output;  nline=nline+1) {
		in  = sigl2r (si, nline)
		if (unitary_greyscale_transformation) {
		    call amovr (Memr[in], Memr[rline], nx_output)
		    call achtrs (Memr[rline], Mems[sline], nx_output)
		} else if (zt == W_LINEAR) {
		    call amapr (Memr[in], Memr[rline], nx_output, z1, z2, dz1, 
			dz2)
		    call achtrs (Memr[rline], Mems[sline], nx_output)
		} else if (zt == W_USER) {
		    call amapr (Memr[in], Memr[rline], nx_output, z1, z2, 
			STARTPT, ENDPT)
		    call achtrs (Memr[rline], Mems[sline], nx_output)
		    call aluts (Mems[sline], Mems[sline], nx_output, Mems[llut])
		}
		
		# Output line to greyscale device
		y1 = ndc_ys + (nline - 1) * delta_y
		y2 = ndc_ys + (nline * delta_y)

		call gpcell (gp, Mems[sline], nx_output, 1, ndc_xs, y1, 
		    ndc_xe, y2)
	    }
	}

	# Free allocate memory
	call sigl2_free (si)
	call sfree (sp)
end
