# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

define	DUMMY	6

include	<error.h>
include	<gset.h>
include	<config.h>
include	<mach.h>
include	<imhdr.h>
include	<xwhen.h>
include	<fset.h>

# T_CONTOUR -- Draw a contour map of a function of two variables.  This is an
# interface to the NCAR CONREC routine.  Rewritten 8/85 to utilize the NCAR
# GKS based utilities.  User has control over device viewport, labelling
# perimeter drawing.  This routine also automatically subsamples or block
# averages to the user specified resolution.

procedure t_contour()

bool	perimeter
char	imsect[SZ_FNAME], label[SZ_LINE]
char	device[SZ_FNAME], title[SZ_LINE], system_id[SZ_LINE]
pointer	im, subras
int	tcojmp[LEN_JUMPBUF]
int	ncols, nlines, epa, status, wkid
int	nset, ncontours, dashpat, mode, nhi, old_onint
int	xres, yres, x_sample, y_sample, subsample
real	interval, floor, ceiling, zero, finc, ybot
real	vx1, vx2, vy1, vy2, wx1, wx2, wy1, wy2
real	first_col, last_col, first_row, last_row

pointer	gp, gopen()

bool	clgetb(), streq(), fp_equalr()
int	clgeti(), btoi()
real	clgetr()
extern	tco_onint()
pointer	immap(), imgs2r()
common	/tcocom/ tcojmp

int	isizel, isizem, isizep, nrep, ncrt, ilab, nulbll, ioffd
int	ioffm, isolid, nla, nlm
real	xlt, ybt, side, ext, hold[5]
common  /conre4/ isizel, isizem , isizep, nrep, ncrt, ilab, nulbll, 
            ioffd, ext, ioffm, isolid, nla, nlm, xlt, ybt, side
int	first
common  /conflg/ first
common  /noaolb/ hold
begin
	# First of all, intialize conrec's block data before altering any
	# parameters in common.
	first = 1
	call conbd

	# Get image section string and output device.
	call clgstr ("image", imsect, SZ_FNAME)
	call clgstr ("device", device, SZ_FNAME)

	zero	= clgetr ("zero")
	floor	= clgetr ("floor")
	ceiling	= clgetr ("ceiling")
	nhi	= clgeti ("nhi")
	dashpat	= clgeti ("dashpat")
	call clgstr ("title", title, SZ_LINE)

	# The user can suppress the contour labelling by setting the common
	# parameter "ilab" to zero.
	if (btoi (clgetb ("label")) == NO)
	    ilab = 0
	else
	    ilab = 1

	# The floor and ceiling are in absolute units, but the zero shift is
	# applied first, so correct the numbers for the zero shift.  Zero is
	# a special number for the floor and ceiling, so do not change value
	# if set to zero.

	if (abs (floor) > EPSILON)
	    floor = floor - zero
	if (abs (ceiling) > EPSILON)
	    ceiling = ceiling - zero

	# User can specify either the number of contours or the contour
	# interval, or let conrec pick a nice number.  Get params and
	# encode the FINC param expected by conrec.

	ncontours = clgeti ("ncontours")
	if (ncontours <= 0) {
	    interval = clgetr ("interval")
	    if (interval <= 0)
		finc = 0
	    else
		finc = interval
	} else
	    finc = - abs (ncontours)

	mode = NEW_FILE
	if (clgetb ("append"))
	    mode = APPEND

	# Map image.
	im = immap (imsect, READ_ONLY, 0)

	ncols  = IM_LEN(im,1)
	nlines = IM_LEN(im,2)
	first_col = 1.0
	last_col = real (ncols)
	first_row = 1.0
	last_row = real (nlines)

	xres = clgeti ("xres")
	yres = clgeti ("yres")

	# Does images resolution need to be decreased?  It does if the number
	# of rows or columns exceeds the x or y resolution and the user
	# hasn't prevented subsampling by setting xres or yres to 0.

	if (ncols > xres && xres != 0) 
	    # Need to decrease resolution in x
	    x_sample = (ncols + xres - 1) / xres
	else
	    x_sample = 1

	if (nlines > yres && yres != 0)
	    # Need to decrease resolution in y
	    y_sample = (nlines + yres - 1) / yres
	else
	    y_sample = 1

	subsample = btoi (clgetb ("subsample"))
	if (x_sample > 1 || y_sample > 1) 
	    call plt_getdata (im, subsample, imsect, x_sample, y_sample, 
		subras, ncols, nlines, clgetb("preserve"))
	else
	    subras = imgs2r (im, 1, ncols, 1, nlines)

	if (streq (title, "imtitle")) {
	    call strcpy (imsect, title, SZ_LINE)
	    call strcat (": ", title, SZ_LINE)
	    call strcat (IM_TITLE(im), title, SZ_LINE)
	}

	# Apply the zero point shift.
	if (abs (zero) > EPSILON)
	    call asubkr (Memr[subras], zero, Memr[subras], ncols * nlines)

	# Open device and make contour plot.
	call gopks (STDERR)
	wkid = 1
	gp = gopen (device, mode, STDGRAPH)
	call gopwk (wkid, DUMMY, gp)
	call gacwk (wkid)

	# The viewport can be set by the user.  If not, the viewport is
	# assumed to be centered on the device.  In either case, the
	# viewport to window mapping is established in pl_map_viewport
	# and conrec's automatic mapping scheme is avoided by setting nset=1.
	vx1 = clgetr ("vx1")
	vx2 = clgetr ("vx2")
	vy1 = clgetr ("vy1")
	vy2 = clgetr ("vy2")
	call pl_map_viewport (gp, ncols,nlines, vx1,vx2,vy1,vy2, clgetb("fill"))
	nset = 1

	perimeter = clgetb ("perimeter")
	if (perimeter)
	    # Suppress conrec's plot label generation 
	    ioffm = 1
	else {
	    # Draw plain old conrec perimeter, set ioffm = 0 to enable label
	    ioffm = 0
	    nset = -1
	    call perim  (ncols - 1, 1, nlines - 1, 1)
	}

	# Install interrupt exception handler.
	call zlocpr (tco_onint, epa)
	call xwhen (X_INT, epa, old_onint)

	# Make the contour plot.  If an interrupt occurs ZSVJMP is reeentered
	# with an error status.
	call zsvjmp (tcojmp, status)
	if (status == OK) {
	    call conrec (Memr[subras], ncols, ncols, nlines,
		floor, ceiling, finc, nset, nhi, -dashpat)
	} else {
	    call gcancel (gp)
	    call fseti (STDOUT, F_CANCEL, OK)
	}

	# Now find window and output text string title.  The window is
	# set to the full image coordinates for labelling.
	call gswind (gp, first_col, last_col, first_row, last_row)
	if (perimeter) 
	    call draw_perimeter (gp)

	call ggview (gp, wx1, wx2, wy1, wy2)
	call gseti (gp, G_WCS, 0)
	ybot = min (wy2 + .06, 0.99)
	call gtext (gp, (wx1 + wx2) / 2.0, ybot, title, "h=c;v=t;f=b;s=.7")

	# Add system id banner to plot
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
	call gclwk (wkid)
	call gclks ()
	call imunmap (im)

	if (subsample == NO && (x_sample > 1 || y_sample > 1)) 
	    # Free space needed for scaled input routines
	    call mfree (subras, TY_REAL)
end


# TCO_ONINT -- Interrupt handler for the task contour.  Branches back to ZSVJMP
# in the main routine to permit shutdown without an error message.

procedure tco_onint (vex, next_handler)

int	vex		# virtual exception
int	next_handler	# not used

int	tcojmp[LEN_JUMPBUF]
common	/tcocom/ tcojmp

begin
	call xer_reset()
	call zdojmp (tcojmp, vex)
end
