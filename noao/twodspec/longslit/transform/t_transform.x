include	<imhdr.h>
include	<math/iminterp.h>
include	<units.h>

define	ITYPES	"|nearest|linear|poly3|poly5|spline3|"

# T_TRANSFORM -- Transform longslit images.
# Input consists of images to be transformed, the user coordinate surfaces
# describing the output coordinates in terms of the input coordinates,
# and the desired coordinates for the output images.  The type of image
# interpolation is also input.  There is a log output as well as the
# transformed images.  The output image may replace the input image.

procedure t_transform ()

int	input			# List of input images
int	output			# List of output images
int	minput			# List of input masks
int	moutput			# List of output masks
int	fitnames		# List of user coordinate fits
pointer	database		# Database
char	interp[10]		# Interpolation type
int	logfiles		# List of log files

int	itypes[II_NTYPES2D], logfd, nusf, nvsf
pointer	in, out, pmin, pmout
pointer	un[2], mw, ct, usf, vsf, xmsi, ymsi, jmsi, xout, yout, dxout, dyout
pointer	sp, image1, image2, image3, minname, moutname, mname, str

int	clpopnu(), clgfil(), clplen(), clgeti(), clgwrd(), open()
int	imtopenp(), imtlen(), imtgetim()
bool	clgetb()
real	clgetr()
pointer	immap(), mw_openim(), yt_mappm()
errchk	tr_gsf, tr_setup, open, mw_openim, yt_mappm

data	itypes /II_BINEAREST, II_BILINEAR, II_BIPOLY3, II_BIPOLY5,
	II_BISPLINE3, II_SINC, II_LSINC, II_DRIZZLE/

include	"transform.com"


begin
	call smark (sp)
	call salloc (database, SZ_FNAME, TY_CHAR)
	call salloc (image1, SZ_FNAME, TY_CHAR)
	call salloc (image2, SZ_FNAME, TY_CHAR)
	call salloc (image3, SZ_FNAME, TY_CHAR)
	call salloc (minname, SZ_FNAME, TY_CHAR)
	call salloc (moutname, SZ_FNAME, TY_CHAR)
	call salloc (mname, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Get and error check the input and output image lists and the other
	# task parameters.

	input = imtopenp ("input")
	output = imtopenp ("output")
	if (imtlen (input) != imtlen (output)) {
	    call imtclose (input)
	    call imtclose (output)
	    call error (1, "Number of input and output images differ")
	}
	minput = imtopenp ("minput")
	moutput = imtopenp ("moutput")
	if (imtlen (minput) > 1 && imtlen (minput) != imtlen (input)) {
	    call imtclose (input)
	    call imtclose (output)
	    call imtclose (minput)
	    call imtclose (moutput)
	    call error (1, "Can't associate input masks with input images")
	}
	if (imtlen (moutput) > 0 && imtlen (input) != imtlen (moutput)) {
	    call imtclose (input)
	    call imtclose (output)
	    call imtclose (minput)
	    call imtclose (moutput)
	    call error (1, "Number output masks differ from input")
	}

	fitnames = clpopnu ("fitnames")
	call clgstr ("database", Memc[database], SZ_FNAME)
	itype = itypes[clgwrd ("interptype", interp, 10, II_FUNCTIONS)]
	logfiles = clpopnu ("logfiles")

	u1 = clgetr ("x1")
	u2 = clgetr ("x2")
	du = clgetr ("dx")
	nu = clgeti ("nx")
	v1 = clgetr ("y1")
	v2 = clgetr ("y2")
	dv = clgetr ("dy")
	nv = clgeti ("ny")

	ulog = clgetb ("xlog")
	vlog = clgetb ("ylog")
	flux = clgetb ("flux")
	blank = clgetr ("blank")

	usewcs = (clplen (fitnames) == 0)

	# Transform each input image to the output image.
	Memc[minname] = EOS
	Memc[moutname] = EOS
	Memc[mname] = EOS
	xmsi = NULL
	while ((imtgetim (input, Memc[image1], SZ_FNAME) != EOF)  &&
	    (imtgetim (output, Memc[image2], SZ_FNAME) != EOF)) {

	    # Get mask names.
	    if (imtgetim (minput, Memc[image3], SZ_FNAME) != EOF)
	        call strcpy (Memc[image3], Memc[minname], SZ_FNAME)
	    if (imtgetim (moutput, Memc[image3], SZ_FNAME) != EOF)
	        call strcpy (Memc[image3], Memc[moutname], SZ_FNAME)

	    # Map the input and output images.
	    call xt_mkimtemp (Memc[image1], Memc[image2], Memc[image3],SZ_FNAME)
	    in = immap (Memc[image1], READ_ONLY, 0)
	    out = immap (Memc[image2], NEW_COPY, in)

	    # Map masks.
	    pmin = NULL; pmout = NULL
	    if (Memc[minname] != EOS)
	        pmin = yt_mappm (Memc[minname], in, "logical", Memc[mname],
		    SZ_FNAME)
	    if (Memc[moutname] != EOS) {
	        call xt_maskname (Memc[moutname], "", NEW_IMAGE,
		    Memc[moutname], SZ_FNAME)
		pmout = immap (Memc[moutname], NEW_COPY, in)
		call imastr (out, "BPM", Memc[moutname])
	    }

	    # Get the coordinate transformation surfaces from the database
	    # and setup the transformations.
	    # Do this only on the first pass.

	    if (xmsi == NULL) {
		if (usewcs) {
		    mw = mw_openim (in)
		    call tr_gwcs (mw, un, IM_LEN(in,1), IM_LEN(in,2), ct,
			usf, nusf, vsf, nvsf)
		} else {
		    mw = NULL
		    ct = NULL
		    call tr_gsf (Memc[database], fitnames, un, usf, nusf,
		        vsf, nvsf)
		}
		call tr_setup (ct, usf, nusf, vsf, nvsf, un, xmsi, ymsi, jmsi,
		    xout, yout, dxout, dyout)
		if (mw != NULL)
		    call mw_close (mw)
	    }

	    # Write log information.
	    while (clgfil (logfiles, Memc[str], SZ_LINE) != EOF) {
		logfd = open (Memc[str], APPEND, TEXT_FILE)
		call sysid (Memc[str], SZ_LINE)
		call fprintf (logfd, "\n%s\n")
		    call pargstr (Memc[str])
		call fprintf (logfd, "  Transform %s to %s.\n")
		    call pargstr (Memc[image1])
		    call pargstr (Memc[image3])
		if (pmout != EOS) {
		    if (pmin != EOS) {
		        call fprintf (logfd, "  Transform mask %s to %s.\n")
			    call pargstr (Memc[mname])
			    call pargstr (Memc[moutname])
		    } else {
		        call fprintf (logfd, "  Output mask is %s.\n")
			    call pargstr (Memc[moutname])
		    }
		}
		if (flux)
		    call fprintf (logfd, "  Conserve flux per pixel.\n")
		if (usewcs)
		    call fprintf (logfd, "  Transforming using image WCS.\n")
		else {
		    call fprintf (logfd, "  User coordinate transformations:\n")
		    while (clgfil (fitnames, Memc[str], SZ_LINE) != EOF) {
			call fprintf (logfd, "    %s\n")
			    call pargstr (Memc[str])
		    }
		}
		call fprintf (logfd, "  Interpolation is %s.\n")
		    call pargstr (interp)
		if (!IS_INDEFR(blank)) {
		    call fprintf (logfd, "  Out of bounds pixel value is %g.\n")
			call pargr (blank)
		} else
		    call fprintf (logfd,
		    "  Using edge extension for out of bounds pixel values.\n")
		call fprintf (logfd, "  Output coordinate parameters are:\n")
		call fprintf (logfd,
	    "    x1 = %10.4g, x2 = %10.4g, dx = %10.4g, nx = %4d, xlog = %b\n")
		    call pargr (u1)
		    call pargr (u2)
		    call pargr (du)
		    call pargi (nu)
		    call pargb (ulog)
		call fprintf (logfd,
	    "    y1 = %10.4g, y2 = %10.4g, dy = %10.4g, ny = %4d, ylog = %b\n")
		    call pargr (v1)
		    call pargr (v2)
		    call pargr (dv)
		    call pargi (nv)
		    call pargb (vlog)
		call close (logfd)
	    }
	    call clprew (logfiles)

	    call tr_transform (in, out, pmin, pmout, un, xmsi, ymsi, jmsi,
	        Memr[xout], Memr[yout], Memr[dxout], Memr[dyout])

	    if (pmout != NULL)
	        call imunmap (pmout)
	    if (pmin != NULL)
	        call xt_pmunmap (pmin)
	    call imunmap (in)
	    call imunmap (out)
	    call xt_delimtemp (Memc[image2], Memc[image3])

	    if (usewcs) {
		call mfree (xout, TY_REAL)
		call mfree (yout, TY_REAL)
		call mfree (dxout, TY_REAL)
		call mfree (dyout, TY_REAL)
		if (xmsi != NULL)
		    call msifree (xmsi)
		if (ymsi != NULL)
		    call msifree (ymsi)
		if (jmsi != NULL)
		    call msifree (jmsi)
		if (un[1] != NULL)
		    call un_close (un[1])
		if (un[2] != NULL)
		    call un_close (un[2])
		xmsi = NULL
	    }

	}

	call mfree (xout, TY_REAL)
	call mfree (yout, TY_REAL)
	call mfree (dxout, TY_REAL)
	call mfree (dyout, TY_REAL)
	if (xmsi != NULL)
	    call msifree (xmsi)
	if (ymsi != NULL)
	    call msifree (ymsi)
	if (jmsi != NULL)
	    call msifree (jmsi)
	if (un[1] != NULL)
	    call un_close (un[1])
	if (un[2] != NULL)
	    call un_close (un[2])
	call imtclose (minput)
	call imtclose (moutput)
	call imtclose (input)
	call imtclose (output)
	call clpcls (fitnames)
	call clpcls (logfiles)
	call sfree (sp)
end


# TR_SETOUTPUT -- Set the output coordinates in the common block.
# This procedure allows the user to specifying a part of the output
# coordinates and let the rest default based on the full limits of
# the user coordinate surfaces.

procedure tr_setoutput (xmin, xmax, ymin, ymax, umin, umax, vmin, vmax)

real	xmin, xmax, ymin, ymax
real	umin, umax, vmin, vmax

int	nua, nva
real	u1a, u2a, dua, v1a, v2a, dva

include	"transform.com"

begin
	# Save the original values of the user parameters.
	u1a = u1
	u2a = u2
	dua = du
	nua = nu
	v1a = v1
	v2a = v2
	dva = dv
	nva = nv

	# If the output coordinate limits are not defined then use the
	# transformation surface limits.

	if (IS_INDEF (u1))
	    u1 = umin
	if (IS_INDEF (u2))
	    u2 = umax
	if (IS_INDEF (v1))
	    v1 = vmin
	if (IS_INDEF (v2))
	    v2 = vmax

	# If the number of output pixels are not defined then use the number
	# of pixels in the input image.

	if (IS_INDEFI (nu))
	    nu = xmax - xmin + 1
	if (IS_INDEFI (nv))
	    nv = ymax - ymin + 1

	# If the coordinate interval is not defined determine it from the
	# number of pixels and the coordinate limits.  If the interval is
	# defined then override the number of pixels.

	if (ulog) {
	    if (IS_INDEF (du))
		du = (log10 (u2) - log10 (u1)) / (nu - 1)
	    else if (IS_INDEFI (nua))
	        nu = nint ((log10 (u2) - log10 (u1)) / du + 1)
	    else if (IS_INDEF (u1a))
		u1 = 10.0 ** (log10 (u2) - du * (nu - 1))
	    else
		u2 = 10.0 ** (log10 (u1) + du * (nu - 1))
	} else {
	    if (IS_INDEF (du))
	        du = (u2 - u1) / (nu - 1)
	    else if (IS_INDEFI (nua))
	        nu = nint ((u2 - u1) / du + 1)
	    else if (IS_INDEF (u1a))
		u1 = u2 - du * (nu - 1)
	    else
		u2 = u1 + du * (nu - 1)
	}

	if (vlog) {
	    if (IS_INDEF (dv))
		dv = (log10 (v2) - log10 (v1)) / (nv - 1)
	    else if (IS_INDEFI (nva))
	        nv = nint ((log10 (v2) - log10 (v1)) / dv + 1)
	    else if (IS_INDEF (v1a))
		v1 = 10.0 ** (log10 (v2) - dv * (nv - 1))
	    else
		v2 = 10.0 ** (log10 (v1) + dv * (nv - 1))
	} else {
	    if (IS_INDEF (dv))
	        dv = (v2 - v1) / (nv - 1)
	    else if (IS_INDEFI (nva))
	        nv = nint ((v2 - v1) / dv + 1)
	    else if (IS_INDEF (v1a))
		v1 = v2 - dv * (nv - 1)
	    else
		v2 = v1 + dv * (nv - 1)
	}
end


define	NBUF		16	# Additional buffer for interpolation
define	NEDGE		2	# Number of edge lines to add for interpolation
define	MINTERP		100	# Mask value for input mask interpolation
define	MTHRESH		10	# Interpolated mask value for bad pixels
define	MBAD		1	# Mask value for output bad pixels
define	MBLANK		1	# Mask value for out of bounds pixels

# TR_TRANSFORM -- Perform the image transformation using a user specified
# image interpolator.  If an input and output mask are included the input
# mask values are set to MINTERP, interpolated in the same way, and any values
# greater than MTHRESH are set to MBAD.  Note that currently the input mask
# values are not used in computing the input data interpolation value.
# The masks MUST be the same size as the input data and are assumed to
# be registered in logical pixel coordinates.

procedure tr_transform (in, out, pmin, pmout, un, xmsi, ymsi, jmsi, xout, yout,
	dxout, dyout)

pointer	in, out			#I IMIO data pointers
pointer	pmin, pmout		#I IMIO mask pointers (NULL if not used)
pointer	un[2]			#I Units
pointer xmsi, ymsi		#I Coordinate interpolation pointers
pointer	jmsi			#I Jacobian interpolation pointer
real	xout[ARB], yout[ARB]	#I Output grid relative to interpolation surface
real	dxout[ARB], dyout[ARB]	#I Output coordinate intervals

int	i, j, nxin, nyin, line1, line2, line3, line4, nlines, laxis, paxis
bool	xofb, yofb
real	a, b, c, r[2], w[2], cd[2,2]
pointer	zmsi, mzmsi, buf, mbuf, bufout
pointer	sp, xin, yin, jbuf, xin1, yin1, y, mw

pointer	mw_open(), impl2r()
errchk	get_daxis

include	"transform.com"

begin
	# Initialize the output image header.

	IM_LEN(out, 1) = nu
	IM_LEN(out, 2) = nv
	if (pmout != NULL) {
	    IM_LEN(pmout, 1) = nu
	    IM_LEN(pmout, 2) = nv
	}

	mw = mw_open (NULL, 2)
	call mw_newsystem (mw, "world", 2)
	do i = 1, 2 {
	    call mw_swtype (mw, i, 1, "linear", "")
	    if (un[i] != NULL) {
		call mw_swattrs (mw, i, "label", UN_LABEL(un[i]))
		call mw_swattrs (mw, i, "units", UN_UNITS(un[i]))
	    }
	}

	r[1] = 1.
	if (ulog)
	    w[1] = log10 (u1)
	else
	    w[1] = u1
	cd[1,1] = du
	cd[1,2] = 0.
	r[2] = 1.
	if (vlog)
	    w[2] = log10 (v1)
	else
	    w[2] = v1
	cd[2,2] = dv
	cd[2,1] = 0.
	call mw_swtermr (mw, r, w, cd, 2)

	# The following image parameters are for compatibility with the
	# ONEDSPEC package if using database solutions.

	if (!usewcs) {
	    call imastr (out, "DCLOG1", "Transform")
	    iferr (call imdelf (out, "REFSPEC1"))
		;
	    iferr (call imdelf (out, "REFSPEC2"))
		;
	    call get_daxis (in, laxis, paxis)
	    call imaddi (out, "dispaxis", laxis)
	    switch (laxis) {
	    case 1:
		if (ulog)
		    call imaddi (out, "dc-flag", 1)
		else
		    call imaddi (out, "dc-flag", 0)
		if (un[laxis] == NULL) {
		    call mw_swattrs (mw, laxis, "label", "Wavelength")
		    call mw_swattrs (mw, laxis, "units", "Angstroms")
		}
	    case 2:
		if (vlog)
		    call imaddi (out, "dc-flag", 1)
		else
		    call imaddi (out, "dc-flag", 0)
		if (un[laxis] == NULL) {
		    call mw_swattrs (mw, laxis, "label", "Wavelength")
		    call mw_swattrs (mw, laxis, "units", "Angstroms")
		}
	    }
	}
	call mw_saveim (mw, out)
	if (pmout != NULL)
	    call mw_saveim (mw, pmout)
	call mw_close (mw)

	# Allocate memory for the input coordinates and a vector for the
	# output y coordinates.  Also initialize the image data buffer.

	call smark (sp)
	call salloc (xin, nu, TY_REAL)
	call salloc (yin, nu, TY_REAL)
	call salloc (y, nu, TY_REAL)
	if (flux)
	    call salloc (jbuf, nu, TY_REAL)
	if (!IS_INDEFR(blank) || pmout != NULL) {
	    call salloc (xin1, nu, TY_REAL)
	    call salloc (yin1, nu, TY_REAL)
	}

	buf = NULL
	mbuf = NULL
	nlines = 0

	# Initialize the interpolator.

	call msiinit (zmsi, itype)
	if (pmin != NULL)
	    call msiinit (mzmsi, itype)

	# Do each line of the output image.

	nxin = IM_LEN(in, 1)
	nyin = IM_LEN(in, 2)

	do i = 1, nv {

	    # Evaluate the input coordinates at the output grid for a line
	    # of the output image using the interpolation surfaces.

	    call amovkr (yout[i], Memr[y], nu)
	    if (!IS_INDEFR(blank) || pmout != NULL) {
		call msivector (xmsi, xout, Memr[y], Memr[xin1], nu)
		call msivector (ymsi, xout, Memr[y], Memr[yin1], nu)
		call amovr (Memr[xin1], Memr[xin], nu)
		call amovr (Memr[yin1], Memr[yin], nu)
	    } else {
		call msivector (xmsi, xout, Memr[y], Memr[xin], nu)
		call msivector (ymsi, xout, Memr[y], Memr[yin], nu)
	    }

	    # Determine the coordinate ranges and check for out of bounds.

	    call alimr (Memr[xin], nu, a, b)
	    xofb = (a < 1 || b > nxin)
	    if (xofb) {
		if (a < 1)
		    call arltr (Memr[xin], nu, 1., 1.)
		if (b > nxin)
		    call argtr (Memr[xin], nu, real (nxin), real (nxin))
	    }

	    call alimr (Memr[yin], nu, a, b)
	    yofb = (a < 1 || b > nyin)
	    if (yofb) {
		if (a < 1) {
		    call arltr (Memr[yin], nu, 1., 1.)
		    a = 1.
		    b = max (a, b)
		}
		if (b > nyin) {
		    call argtr (Memr[yin], nu, real (nyin), real (nyin))
		    b = nyin
		    a = min (a, b)
		}
	    }

	    # Get the input image data and fit an interpolator to the data.

	    if ((buf == NULL) || (b > line2) || (a < line1)) {
		nlines = max (nlines, int (b - a + 2 + NBUF))
		if (buf == NULL) {
		    if (a < nyin / 2) {
		        line1 = max (1, int (a))
		        line2 = min (nyin, line1 + nlines - 1)
		    } else {
		        line2 = min (nyin, int (b+1.))
		        line1 = max (1, line2 - nlines + 1)
		    }
		} else if (b > line2) {
		    line1 = max (1, int (a))
		    line2 = min (nyin, line1 + nlines - 1)
		    line1 = max (1, line2 - nlines + 1)
		} else {
		    line2 = min (nyin, int (b+1.))
		    line1 = max (1, line2 - nlines + 1)
		    line2 = min (nyin, line1 + nlines - 1)
		}
		line3 = max (1, line1 - NEDGE)
		line4 = min (nyin, line2 + NEDGE)
	        call tr_bufl2r (in, pmin, line3, line4, buf, mbuf)
	        call msifit (zmsi, Memr[buf], nxin, line4 - line3 + 1, nxin)
		if (pmin != NULL)
		    call msifit (mzmsi, Memr[mbuf], nxin, line4 - line3 + 1,
		        nxin)
	    }

	    # The input coordinates must be offset to interpolation data grid.
	    call asubkr (Memr[yin], real (line3 - 1), Memr[yin], nu)

	    # Evaluate output image pixels, conserve flux (if requested) using
	    # the Jacobian, and set the out of bounds values.

	    bufout = impl2r (out, i)
	    call msivector (zmsi, Memr[xin], Memr[yin], Memr[bufout], nu)
	    if (flux) {
	        call msivector (jmsi, xout, Memr[y], Memr[jbuf], nu)
		call amulr (dxout, Memr[jbuf], Memr[jbuf], nu)
		call amulkr (Memr[jbuf], dyout[i], Memr[jbuf], nu)
	        call amulr (Memr[bufout], Memr[jbuf], Memr[bufout], nu)
	    }
	    if (!IS_INDEFR(blank)) {
		if (xofb) {
		    do j = 0, nu-1 {
			if (Memr[xin1+j] < 1 || Memr[xin1+j] > nxin)
			    Memr[bufout+j] = blank
		    }
		}
		if (yofb) {
		    do j = 0, nu-1 {
			if (Memr[yin1+j] < 1 || Memr[yin1+j] > nyin)
			    Memr[bufout+j] = blank
		    }
		}
	    }

	    # Evaluate output mask pixels and set output bad values.

	    if (pmout != NULL) {
		bufout = impl2r (pmout, i)
		if (pmin != NULL) {
		    call msivector (mzmsi, Memr[xin], Memr[yin], Memr[bufout],
		        nu)
		    do j = 0, nu-1 {
			c = Memr[bufout+j]
			if (Memr[xin1+j] < 1 || Memr[xin1+j] > nxin ||
			    Memr[yin1+j] < 1 || Memr[yin1+j] > nyin)
			    Memr[bufout+j] = MBLANK
			else if (c > 0.) {
			    if (c > MTHRESH)
				Memr[bufout+j] = MBAD
			    else
				Memr[bufout+j] = 0
			}
		    }
		} else {
		    call aclrr (Memr[bufout], nu)
		    if (xofb) {
			do j = 0, nu-1 {
			    if (Memr[xin1+j] < 1 || Memr[xin1+j] > nxin)
				Memr[bufout+j] = MBLANK
			}
		    }
		    if (yofb) {
			do j = 0, nu-1 {
			    if (Memr[yin1+j] < 1 || Memr[yin1+j] > nyin)
				Memr[bufout+j] = MBLANK
			}
		    }
		}
	    }
	}

	# Free memory.

	call mfree (buf, TY_REAL)
	call mfree (mbuf, TY_REAL)
	call msifree (zmsi)
	if (pmin != NULL)
	    call msifree (mzmsi)
	call sfree (sp)
end


# TR_BUFL2R -- Maintain buffer of image lines.  A new buffer is created when
# the buffer pointer is null or if the number of lines requested is changed.
# The minimum number of image reads is used.

procedure tr_bufl2r (im, pmin, line1, line2, buf, mbuf)

pointer	im		#I Image pointer
pointer	pmin		#I Mask pointer
int	line1		#I First image line of buffer
int	line2		#I Last image line of buffer
pointer	buf		#U Output data buffer
pointer	mbuf		#U Output mask buffer

int	i, nlines, nx, last1, last2, nlast
pointer	buf1, buf2

pointer	imgl2r()

begin
	nlines = line2 - line1 + 1

	# If the buffer pointer is undefined then allocate memory for the
	# buffer.  If the number of lines requested changes reallocate
	# the buffer.  Initialize the last line values to force a full
	# buffer image read.

	if (buf == NULL) {
	    nx = IM_LEN(im, 1)
	    call malloc (buf, nx * nlines, TY_REAL)
	    if (pmin != NULL)
		call malloc (mbuf, nx * nlines, TY_REAL)
	    last1 = line1 - nlines
	    last2 = line2 - nlines
	} else if (nlines != nlast) {
	    call realloc (buf, nx * nlines, TY_REAL)
	    if (pmin != NULL)
		call realloc (mbuf, nx * nlines, TY_REAL)
	    last1 = line1 - nlines
	    last2 = line2 - nlines
	}

	# Read only the image lines with are different from the last buffer.

	if (line1 < last1) {
	    do i = line2, line1, -1 {
		if (i > last1)
		    buf1 = buf + (i - last1) * nx
		else
		    buf1 = imgl2r (im, i)
		    
		buf2 = buf + (i - line1) * nx
		call amovr (Memr[buf1], Memr[buf2], nx)
	    }
	} else if (line2 > last2) {
	    do i = line1, line2 {
		if (i < last2)
		    buf1 = buf + (i - last1) * nx
		else
		    buf1 = imgl2r (im, i)
		    
		buf2 = buf + (i - line1) * nx
		call amovr (Memr[buf1], Memr[buf2], nx)
	    }
	}
	if (pmin != NULL) {
	    if (line1 < last1) {
		do i = line2, line1, -1 {
		    if (i > last1)
			buf1 = mbuf + (i - last1) * nx
		    else
			buf1 = imgl2r (pmin, i)
			
		    buf2 = mbuf + (i - line1) * nx
		    call amovr (Memr[buf1], Memr[buf2], nx)
		    call argtr (Memr[buf2], nx, 0.1, real(MINTERP))
		}
	    } else if (line2 > last2) {
		do i = line1, line2 {
		    if (i < last2)
			buf1 = mbuf + (i - last1) * nx
		    else
			buf1 = imgl2r (pmin, i)
			
		    buf2 = mbuf + (i - line1) * nx
		    call amovr (Memr[buf1], Memr[buf2], nx)
		    call argtr (Memr[buf2], nx, 0.1, real(MINTERP))
		}
	    }
	}

	# Save the buffer parameters.

	last1 = line1
	last2 = line2
	nlast = nlines
end
