# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<gset.h>
include	<config.h>
include	<mach.h>
include	<imhdr.h>
include	<xwhen.h>
include	<fset.h>

define	DUMMY	6
define	SAMPLE_SIZE	1000
define	LEN_STDLINE	40

# HAFTON -- Draw a half tone plot of an image section.  This is an
# interface to the NCAR HAFTON routine.  

procedure t_hafton()

int	sign
bool	sub, pre
pointer	im, subras, gp
int	tcojmp[LEN_JUMPBUF]
char	imsect[SZ_FNAME], mapping_function[SZ_FNAME]
char	device[SZ_FNAME], title[SZ_LINE], system_id[SZ_LINE]
int	ncols, nlines, epa, status, wkid, mode, old_onint
int	nlevels, nprm, nopt, xres, yres, nfunction, nx, ny
real	z1, z2, wx1, wx2, wy1, wy2, contrast
real	xs, xe, ys, ye, vx1, vx2, vy1, vy2

real	clgetr()
extern	hf_tco_onint()
int	clgeti(),  strncmp()
pointer	gopen(), plt_getdata(), immap()
bool	clgetb(), fp_equalr(), streq()
common	/tcocom/ tcojmp

begin
	# Get image section string and output device.
	call clgstr ("image", imsect, SZ_FNAME)
	call clgstr ("device", device, SZ_FNAME)

	# Map image.
	im = immap (imsect, READ_ONLY, 0)

	z1 = clgetr ("z1")
	z2 = clgetr ("z2")

	# Gaurantee that image min/max is up to date
	if (IM_LIMTIME(im) < IM_MTIME(im))
	    call hf_minmax (im, IM_MIN(im), IM_MAX(im))

	if (fp_equalr (z1, z2)) {
	    z1 = IM_MIN(im)
	    z2 = IM_MAX(im)
	}

	# User can specify the type of mapping function used, and whether
	# the contrast is negative or positive.  

	nlevels = clgeti ("nlevels")
	contrast = clgetr ("contrast")
	call clgstr ("mapping_function", mapping_function, SZ_FNAME)

	# Assign integer code to specified mapping function
	if (strncmp (mapping_function, "linear", 2) == 0)
	    nfunction = 1
	else if (strncmp (mapping_function, "exponential", 1) == 0)
	    nfunction = 2
	else if (strncmp (mapping_function, "logarithmic", 2) == 0)
	    nfunction = 3
	else if (strncmp (mapping_function, "sinusoidal",  1) == 0)
	    nfunction = 4
	else if (strncmp (mapping_function, "arcsine",     1) == 0)
	    nfunction = 5
	else if (strncmp (mapping_function, "crtpict",     1) == 0)
	    nfunction = 6
	else
	    call error (0, "Hafton: unknown mapping function")

	sign = 1.0
	if (contrast < 0.0)
	    sign = -1.0
	nopt = sign * nfunction

	mode = NEW_FILE
	if (clgetb ("append"))
	    mode = APPEND

	# Read in subraster.  Image resolution can be decreased by
	# subsampling or block averaging.

	xres = clgeti ("xres")
	yres = clgeti ("yres")
	sub = clgetb ("subsample")
	pre = clgetb ("preserve")

	# Retrieve values from image header that will be needed.
	ncols = IM_LEN(im,1)
	nlines = IM_LEN(im,2)
	if (streq (title, "imtitle")) {
	    call strcpy (imsect, title, SZ_LINE)
	    call strcat (": ", title, SZ_LINE)
	    call strcat (IM_TITLE(im), title, SZ_LINE)
	}

	xs = 1.0
	xe = real (ncols)
	ys = 1.0
	ye = real (nlines)

	# Get data with proper resolution.  Procedure plt_getdata returns
	# a pointer to the data matrix to be contoured.  The resolution
	# is decreased by the specified mathod in this procedure.  The
	# dimensions of the data array are also returned.  The image
	# header pointer can be unmapped after plt_getdata is called.

	nx = 0
	ny = 0
	subras = plt_getdata (im, sub, pre, xres, yres, nx, ny)

	if (nfunction == 6) {
	    # User wants crtpict automatic algorithm - linear mapping
	    # between calculated z1, z2 using possible non-integer contrast.
	    # Get z1, z2 as if positive contrast.  Set nopt later to negative
	    # if necessary.

	    call zscale (im, z1, z2, abs(contrast), SAMPLE_SIZE, LEN_STDLINE)
	}

	call eprintf ("Intensities from z1=%.2f to z2=%.2f mapped with a")
    	    call pargr (z1)
    	    call pargr (z2)

        switch (nfunction) {
        case (1):
	    call eprintf (" linear function\n")
        case (2):
	    call eprintf ("n exponential function\n")
        case (3):
	    call eprintf (" logarithmic function\n")
        case (4):
	    call eprintf (" sinusodial function\n")
        case (5):
	    call eprintf ("n arcsine function\n")
        case (6):
	    call eprintf (" CRTPICT function\n")
	    if (nopt > 0) {
		# Positive contrast.  Set nopt to positive linear mapping.
	        nopt = 1
	    } else  {
		# Negative contrast. Set nopt to negative linear mapping.
		nopt = -1
	    }
        }

	vx1 = clgetr ("vx1")
	vx2 = clgetr ("vx2")
	vy1 = clgetr ("vy1")
	vy2 = clgetr ("vy2")

	# Open device and make contour plot.
	call gopks (STDERR)
	wkid = 1
	gp = gopen (device, mode, STDGRAPH)
	call gopwk (wkid, DUMMY, gp)
	call gacwk (wkid)

	call pl_map_viewport (gp,
	    ncols, nlines, vx1, vx2, vy1, vy2, clgetb ("fill"), true)
	nprm = -1

	# Install interrupt exception handler.
	call zlocpr (hf_tco_onint, epa)
	call xwhen (X_INT, epa, old_onint)

	# Make the hafton plot.  If an interrupt occurs ZSVJMP is reentered
	# with an error status.

	call zsvjmp (tcojmp, status)
	if (status == OK) {
	    call hafton (Memr[subras], nx, nx, ny, z1, z2,
	        nlevels, nopt, nprm, 0, 0.)
	} else {
	    call gcancel (gp)
	    call fseti (STDOUT, F_CANCEL, OK)
	}

	# Should a fancy (crtpict like) perimeter be drawn around the plot?
	if (clgetb ("perimeter")) {
	    call gswind (gp, xs, xe, ys, ye)
	    call draw_perimeter (gp)
	} else
	    call perim (1, ncols - 1, nlines - 1, 1)

	# Now find window and output text string title.  The window is
	# set to the full image coordinates for labelling.

	call ggview (gp, wx1, wx2, wy1, wy2)
	call gseti (gp, G_WCS, 0)
	call gtext (gp, (wx1 + wx2) / 2.0, wy2 + .03, title, "h=c;v=b;f=b;s=.7")

	# Add system id banner to plot.
	call gseti (gp, G_CLIP, NO)
	call sysid (system_id, SZ_LINE)
	call gtext (gp, (wx1+wx2)/2.0, wy1-0.07, system_id, "h=c;v=b;s=.5")

	call gdawk (wkid)
	call gclwk (wkid)
	call gclks ()
	call imunmap (im)

	# Free space used for scaled input routines.
	call mfree (subras, TY_REAL)
end


# HF_TCO_ONINT -- Interrupt handler for the task hafton.  Branches back to 
# ZSVJMP in the main routine to permit shutdown without an error message.

procedure hf_tco_onint (vex, next_handler)

int	vex		# virtual exception
int	next_handler	# not used

int	tcojmp[LEN_JUMPBUF]
common	/tcocom/ tcojmp

begin
	call xer_reset()
	call zdojmp (tcojmp, vex)
end


# HF_MINMAX -- Compute the minimum and maximum pixel values of an image.
# Works for images of any dimensionality, size, or datatype, although
# the min and max values can currently only be stored in the image header
# as real values.

procedure hf_minmax (im, min_value, max_value)

pointer	im				# image descriptor
real	min_value			# minimum pixel value in image (out)
real	max_value			# maximum pixel value in image (out)

pointer	buf
bool	first_line
long	v[IM_MAXDIM]
short	minval_s, maxval_s
long	minval_l, maxval_l
real	minval_r, maxval_r
int	imgnls(), imgnll(), imgnlr()
errchk	amovkl, imgnls, imgnll, imgnlr, alims, aliml, alimr

begin
	call amovkl (long(1), v, IM_MAXDIM)		# start vector
	first_line = true
	min_value = INDEF
	max_value = INDEF

	switch (IM_PIXTYPE(im)) {
	case TY_SHORT:
	    while (imgnls (im, buf, v) != EOF) {
		call alims (Mems[buf], IM_LEN(im,1), minval_s, maxval_s)
		if (first_line) {
		    min_value = minval_s
		    max_value = maxval_s
		    first_line = false
		} else {
		    if (minval_s < min_value)
			min_value = minval_s
		    if (maxval_s > max_value)
			max_value = maxval_s
		}
	    }
	case TY_USHORT, TY_INT, TY_LONG:
	    while (imgnll (im, buf, v) != EOF) {
		call aliml (Meml[buf], IM_LEN(im,1), minval_l, maxval_l)
		if (first_line) {
		    min_value = minval_l
		    max_value = maxval_l
		    first_line = false
		} else {
		    if (minval_l < min_value)
			min_value = minval_l
		    if (maxval_l > max_value)
			max_value = maxval_l
		}
	    }
	default:
	    while (imgnlr (im, buf, v) != EOF) {
		call alimr (Memr[buf], IM_LEN(im,1), minval_r, maxval_r)
		if (first_line) {
		    min_value = minval_r
		    max_value = maxval_r
		    first_line = false
		} else {
		    if (minval_r < min_value)
			min_value = minval_r
		    if (maxval_r > max_value)
			max_value = maxval_r
		}
	    }
	}
end
