include	<error.h>
include	<mach.h>
include	<gset.h>
include	<config.h>
include	<xwhen.h>
include	<fset.h>
include "../lib/daophotdef.h"
include "../lib/psfdef.h"

define	DUMMY		6
define	XCEN	        0.5
define	YCEN		0.52
define	EDGE1		0.1
define	EDGE2		0.93
define	SZ_LABEL	10
define	SZ_FMT		20

# DP_CONTPSF -- Draw a contour plot of a data subraster containing. The
# data floor and ceiling are set by the user, but the contour interval
# is chosen by the routine.

procedure dp_contpsf (dao, subras, ncols, nlines, title, gp)

pointer	dao				# pointer to DAOPHOT structure
real	subras[ncols,nlines]		# data subraster
int	ncols, nlines			# dimesnions of subraster
char	title[ARB]			# title string
pointer	gp				# pointer to graphics descriptor

bool	perimeter
char	system_id[SZ_LINE], label[SZ_LINE]
int	epa, status, old_onint, tcojmp[LEN_JUMPBUF]
int	wkid, nset, ncontours, dashpat, nhi
pointer	sp, temp, psf
real	interval, floor, ceiling, zero, finc, ybot
real	vx1, vx2, vy1, vy2, wx1, wx2, wy1, wy2
real	first_col, last_col, first_row, last_row

bool	fp_equalr()
extern	dp_conint()
common	/tcocom/ tcojmp

int	first
int	isizel, isizem, isizep, nrep, ncrt, ilab, nulbll, ioffd
int	ioffm, isolid, nla, nlm
real	xlt, ybt, side, ext, hold[5]
common  /conflg/ first
common  /conre4/ isizel, isizem , isizep, nrep, ncrt, ilab, nulbll, 
            ioffd, ext, ioffm, isolid, nla, nlm, xlt, ybt, side
common  /noaolb/ hold

begin
	# Get the pointer to the DAOPHOT PSF fitting substructure.
	psf = DP_PSF (dao)

	# First of all, intialize conrec's block data before altering any
	# parameters in common.
	first = 1
	call conbd

	# Set local variables.
	zero	= 0.0
	floor	= DP_CFLOOR (psf)
	ceiling	= DP_CCEILING (psf)
	nhi	= -1
	dashpat	= 528

	# Suppress the contour labelling by setting the common
	# parameter "ilab" to zero.
	ilab = 0

	# The floor and ceiling are in absolute units, but the zero shift is
	# applied first, so correct the numbers for the zero shift.  Zero is
	# a special number for the floor and ceiling, so do not change value
	# if set to zero.

	if (abs (floor) > EPSILON)
	    floor = floor - zero
	if (abs (ceiling) > EPSILON)
	    ceiling = ceiling - zero

	# User can specify either the number of contours or the contour
	# interval, or let conrec pick a nice number.  Set ncontours to 0
	# and encode the FINC param expected by conrec.

	ncontours = 0
	if (ncontours <= 0) {
	    interval = 0
	    if (interval <= 0)
		finc = 0
	    else
		finc = interval
	} else
	    finc = - abs (ncontours)

	# Make a copy of the data and do the contouring on this.
	call smark (sp)
	call salloc (temp, ncols * nlines, TY_REAL)
	call amovr (subras, Memr[temp], nlines * ncols)

	first_col = 1.0
	last_col = real (ncols)
	first_row = 1.0
	last_row = real (nlines)

	# Apply the zero point shift.
	if (abs (zero) > EPSILON)
	    call asubkr (Memr[temp], zero, Memr[temp], ncols * nlines)

	# Open device and make contour plot.
	call gopks (STDERR)
	wkid = 1
	call gclear (gp)
	call gopwk (wkid, DUMMY, gp)
	call gacwk (wkid)

	# The viewport can be set by the user.  If not, the viewport is
	# assumed to be centered on the device.  In either case, the
	# viewport to window mapping is established in dp_map_viewport
	# and conrec's automatic mapping scheme is avoided by setting nset=1.
	vx1 = 0.10
	vx2 = 0.90
	vy1 = 0.10
	vy2 = 0.90
	call dp_map_viewport (gp, ncols, nlines, vx1, vx2, vy1, vy2, false)
	nset = 1

	perimeter = TRUE 
	if (perimeter)
	    # Suppress conrec's plot label generation.
	    ioffm = 1
	else {
	    # Draw plain old conrec perimeter, set ioffm = 0 to enable label.
	    ioffm = 0
	    call perim  (ncols - 1, 1, nlines - 1, 1)
	}

	# Install interrupt exception handler.
	call zlocpr (dp_conint, epa)
	call xwhen (X_INT, epa, old_onint)

	# Make the contour plot.  If an interrupt occurs ZSVJMP is reeentered
	# with an error status.
	call zsvjmp (tcojmp, status)
	if (status == OK) {
	    call conrec (Memr[temp], ncols, ncols, nlines, floor, ceiling,
	        finc, nset, nhi, -dashpat)
	} else {
	    call gcancel (gp)
	    call fseti (STDOUT, F_CANCEL, OK)
	}

	# Now find window and output text string title.  The window is
	# set to the full image coordinates for labelling.
	call gswind (gp, first_col, last_col, first_row, last_row)
	if (perimeter) 
	    call dp_cperimeter (gp)

	call ggview (gp, wx1, wx2, wy1, wy2)
	call gseti (gp, G_WCS, 0)
	ybot = min (wy2 + .06, 0.99)
	call gtext (gp, (wx1 + wx2) / 2.0, ybot, title, "h=c;v=t;f=b;s=.7")

	# Add system id banner to plot.
	call gseti (gp, G_CLIP, NO)
	call sysid (system_id, SZ_LINE)
	ybot = max (wy1 - 0.08, 0.01)
	call gtext (gp, (wx1+wx2)/2.0, ybot, system_id, "h=c;v=b;s=.5")
	
	if (perimeter) {
	    if (fp_equalr (hold(5), 1.0)) {
	        call sprintf (label, SZ_LINE, 
	            "contoured from %g to %g, interval = %g")
	            call pargr (hold(1))
	            call pargr (hold(2))
	            call pargr (hold(3))
	    } else {
	        call sprintf (label, SZ_LINE, 
	  "contoured from %g to %g, interval = %g, labels scaled by %g")
	            call pargr (hold(1))
	            call pargr (hold(2))
	            call pargr (hold(3))
		    call pargr (hold(5))
	    }
	    ybot = max (wy1 - 0.06, .03)
	    call gtext (gp, (wx1 + wx2) / 2.0, ybot, label, "h=c;v=b;s=.6")
	}

	call gswind (gp, first_col, last_col, first_row, last_row)
	call gdawk (wkid)
	call gclks ()
	call sfree (sp)
end


# DP_CONINT -- Interrupt handler for the task contour.  Branches back to
# ZSVJMP in the main routine to permit shutdown without an error message.

procedure dp_conint (vex, next_handler)

int	vex		# virtual exception
int	next_handler	# not used

int	tcojmp[LEN_JUMPBUF]
common	/tcocom/ tcojmp

begin
	call xer_reset()
	call zdojmp (tcojmp, vex)
end


# DP_CPERIMETER -- draw and annotate the axes drawn around the perimeter
# of the image pixels.  The viewport and window have been set by 
# the calling procedure.  Plotting is done in window coordinates.
# This procedure is called by both crtpict and the ncar plotting routines
# contour and hafton.

procedure dp_cperimeter (gp)

pointer	gp			# graphics descriptor
real	xs, xe, ys, ye		# WCS coordinates of pixel window

char	label[SZ_LABEL], fmt1[SZ_FMT], fmt2[SZ_FMT], fmt3[SZ_FMT], fmt4[SZ_FMT]
int	i, first_col, last_col, first_tick, last_tick, bias
int	nchar, first_row, last_row, cnt_step, cnt_label
real	dist, kk, col, row, dx, dy, sz_char, cw, xsz, label_pos
real	xdist, ydist, xspace, yspace, k[3]
bool	ggetb()
int	itoc()
real 	ggetr()
data 	k/1.0,2.0,3.0/
errchk	ggwind, gseti, gctran, gline, gtext, itoc

begin
	# First, get window coordinates and turn off clipping.
	call ggwind (gp, xs, xe, ys, ye)
	call gseti (gp, G_CLIP, NO)

	# A readable character width seems to be about 1.mm.  A readable
	# perimeter seperation seems to be about .80mm.  If the physical
	# size of the output device is contained in the graphcap file, the
	# NDC sizes of these measurements can be determined.  If not, 
	# the separation between perimeter axes equals one quarter character
	# width or one quarter percent of frame, which ever is larger, and 
	# the character size is set to 0.40.

	cw = max (ggetr (gp, "cw"), 0.01)
	if (ggetb (gp, "xs")) {
	    xsz = ggetr (gp, "xs")
	    dist = .80 / (xsz * 1000.)
	    sz_char = dist / cw
	} else {
	    # Get character width and calculate perimeter separation.
	    dist = cw * 0.25
	    sz_char = 0.40
	}

	# Convert distance to user coordinates.
	call ggscale (gp, xs, ys, dx, dy)
	xdist = dist * dx
	ydist = dist * dy

	# Generate four possible format strings for gtext.
	call sprintf (fmt1, SZ_LINE, "h=c;v=t;s=%.2f")
	    call pargr (sz_char)
	call sprintf (fmt2, SZ_LINE, "h=c;v=b;s=%.2f")
	    call pargr (sz_char)
	call sprintf (fmt3, SZ_LINE, "h=r;v=c;s=%.2f")
	    call pargr (sz_char)
	call sprintf (fmt4, SZ_LINE, "h=l;v=c;s=%.2f")
	    call pargr (sz_char)

	# Draw inner and outer perimeter
	kk = k[1]
	do i = 1, 2 {
	    xspace = kk * xdist
	    yspace = kk * ydist
	    call gline (gp, xs - xspace, ys - yspace, xe + xspace, ys - yspace)
	    call gline (gp, xe + xspace, ys - yspace, xe + xspace, ye + yspace)
	    call gline (gp, xe + xspace, ye + yspace, xs - xspace, ye + yspace)
	    call gline (gp, xs - xspace, ye + yspace, xs - xspace, ys - yspace)
	    kk = k[2]
	}

	# Now draw x axis tick marks, along both the bottom and top of
	# the picture.  First find the endpoint integer pixels.

	first_col = int (xs)
	last_col = int (xe)

	# Determine increments of ticks and tick labels for x axis.
	cnt_step  = 1
	cnt_label = 10
	if (last_col - first_col > 256) {
	    cnt_step = 10
	    cnt_label = 100
	} else if (last_col - first_col < 26) {
	    cnt_step = 1
	    cnt_label = 1
	}

	first_tick = first_col
	bias = mod (first_tick, cnt_step)
	last_tick = last_col + bias

	do i = first_tick, last_tick, cnt_step {
	    col = real (i - bias)
	    call gline (gp, col, ys - k[1] * ydist, col, ys - k[2] * ydist)
	    call gline (gp, col, ye + k[1] * ydist, col, ye + k[2] * ydist)

	    if (mod ((i - bias), cnt_label)  == 0) {

		# Label tick mark; calculate number of characters needed.
		nchar = 3
		if (int (col) == 0)
		    nchar = 1
		if (int (col) >= 1000)
		    nchar = 4
		if (itoc (int(col), label, nchar) <= 0)
		    label[1] = EOS

		# Position label slightly below outer perimeter.  Seperation
		# is twenty percent of a character width, in WCS.
		label_pos = ys - (k[2] * ydist + (cw * 0.20 * dy))
		call gtext (gp, col, label_pos, label, fmt1)

		# Position label slightly above outer perimeter.
		label_pos = ye + (k[2] * ydist + (cw * 0.20 * dy))
		call gtext (gp, col, label_pos, label, fmt2)
	    }
	}

	# Label the y axis tick marks along the left and right sides of the
	# picture.  First find the integer pixel endpoints.
	
	first_row = int (ys)
	last_row = int (ye)

	# Determine increments of ticks and tick labels for y axis.
	cnt_step  = 1
	cnt_label = 10
	if (last_row - first_row > 256) {
	    cnt_step = 10
	    cnt_label = 100
	} else if (last_row - first_row < 26) {
	    cnt_step = 1
	    cnt_label = 1
	}

	first_tick = first_row 
	bias = mod (first_tick, cnt_step)
	last_tick = last_row + bias

	do i = first_tick, last_tick, cnt_step {
	    row = real (i - bias)
	    call gline (gp, xs - k[1] * xdist, row, xs - k[2] * xdist, row)
	    call gline (gp, xe + k[1] * xdist, row, xe + k[2] * xdist, row)

	    if (mod ((i - bias), cnt_label) == 0) {

		# Label tick mark; calculate number of characters needed
		nchar = 3
		if (int (row) == 0)
		    nchar = 1
		else if (int (row) >= 1000)
		    nchar = 4
	        if (itoc (int(row), label, nchar) <= 0)
		    label[1] = EOS

		# Position label slightly to the left of outer perimeter.
		# Separation twenty percent of a character width, in WCS.
		label_pos = xs - (k[2] * xdist + (cw * 0.20 * dx))
	        call gtext (gp, label_pos, row, label, fmt3)

		# Position label slightly to the right of outer perimeter.
		label_pos = xe + (k[2] * xdist + (cw * 0.20 * dx))
	        call gtext (gp, label_pos, row, label, fmt4)
	    }
	}
end


# DP_MAP_VIEWPORT -- Set device viewport for contour and hafton plots.  If not
# specified by user, a default viewport centered on the device is used.

procedure dp_map_viewport (gp, ncols, nlines, ux1, ux2, uy1, uy2, fill)

pointer	gp			# graphics pointer
int	ncols			# number of image cols
int	nlines			# number of image lines
real	ux1, ux2, uy1, uy2	# NDC coordinates of requested viewort
bool	fill			# fill viewport (vs enforce unity aspect ratio?)

real	ncolsr, nlinesr, ratio, aspect_ratio
real	x1, x2, y1, y2, ext, xdis, ydis
data    ext /0.25/
bool	fp_equalr()
real	ggetr()

begin
	ncolsr = real (ncols)
	nlinesr = real (nlines)
	if (fp_equalr (ux1, 0.0) && fp_equalr (ux2, 0.0) && 
	    fp_equalr (uy1, 0.0) && fp_equalr (uy2, 0.0)) {

	    x1 = EDGE1
	    x2 = EDGE2
	    y1 = EDGE1
	    y2 = EDGE2

	    # Calculate optimum viewport, as in NCAR's conrec, hafton.
	    ratio = min (ncolsr, nlinesr) / max (ncolsr, nlinesr)
	    if (ratio >= ext) {
		if (ncols > nlines) 
		    y2 = (y2 - y1) * nlinesr / ncolsr + y1
		else 
		    x2 = (x2 - x1) * ncolsr / nlinesr + x1
	    }

	    xdis = x2 - x1
	    ydis = y2 - y1

	    # So far, the viewport has been calculated so that equal numbers of
	    # image pixels map to equal distances in NDC space, regardless of 
	    # the aspect ratio of the device.  If the parameter "fill" has been
	    # set to no, the user wants to compensate for a non-unity aspect 
	    # ratio and make equal numbers of image pixels map to into the same 
	    # physical distance on the device, not the same NDC distance.

	    if (! fill) {
	        aspect_ratio = ggetr (gp, "ar")
	        if (fp_equalr (aspect_ratio, 0.0))
	            aspect_ratio = 1.0
	        xdis = xdis * aspect_ratio
	    }

	    ux1 = XCEN - (xdis / 2.0)
	    ux2 = XCEN + (xdis / 2.0)
	    uy1 = YCEN - (ydis / 2.0)
	    uy2 = YCEN + (ydis / 2.0)
	}

	# Set window and viewport for WCS 1.
	call gseti  (gp, G_WCS, 1)
	call gsview (gp, ux1, ux2, uy1, uy2)
	call gswind (gp, 1.0, ncolsr, 1.0, nlinesr)
	call set (ux1, ux2, uy1, uy2, 1.0, ncolsr, 1.0, nlinesr, 1)
end
