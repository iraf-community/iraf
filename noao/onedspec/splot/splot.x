include	<error.h>
include	<imhdr.h>
include	<gset.h>
include	<pkg/gtools.h>
include "../oned.h"
include "../idsmtn.h"

define	KEY		"noao$lib/scr/splot.key"
define	PROMPT		"splot options"

define	OPTIONS		",auto,zero,"
define	NOPTIONS	2
define	AUTO		1	# Option number for auto graph
define	ZERO		2	# Option number of zero y minimum

# SPLOT -- Plot an image line and play with it - Most appropriate for spectra
#
# NOTE: THIS IS A PRELIMINARY PROGRAM - Some of the functions are
#       not particularly robust and may be replaced in the future.
#       Especially DEBLEND which uses a non-linear least squares
#         routine (simplex search) which is totally opaque but usually 
#         converges well.
#       Also EQUIV. WIDTH which integrates under the line by summation over
#         the pixels. The centering is done by a weighted center of gravity
#
#  Current Keystrokes in addition to IRAF Standard Graphics (capital keys)
#
#	/ - Help on status line
#	? - Help on clear screen
#	a - Autoexpand
#	b - Set base plot level to 0.0
#	c - Print cursor position
#	d - Deblend lines
#	e - Equivalent width, integrated flux, line center
#	f - Function operators
#	g - Get new image
#	h - Equivalent width using half of a blended line (see k)
#	i - Invert spectrum (Flip left for right)
#	j - Fudge a point to Y-cursor value
#	k - Equivalent width as per C. Pilachowski
#	l - Convert to F-lambda
#	m - RMS, mean, signal-to-noise in region
#	n - Convert to F-nu
#	o - Overplot another spectrum
#	p - Convert to wavelength scale
#	q - Quit
#	r - Replot
#	s - Smooth (boxcar)
#	t - flaTTen a spectrum and normalize to average 1.0
#	u - User coordinate computation
#	w - Write current image as new spectrum
#	x - "Etch-a-sketch" mode - connects the cursor positions
#	y - Yank standard star data and overplot
#	z - "Zoom" outward during expansion
#	$ - Convert to channel scale
#	- - (Minus) subtract deblended fit
#	. - Slide spectrum upward
#	, - Slide spectrum downward
#	I - Interrupt

procedure splot ()

int	list
int	i, npts, nline
int	wc, key
int	nans
real	wx, wy
real	x1, x2, dx
real	avg_pix, sigma_pix
char	image[SZ_FNAME], command[SZ_FNAME]
char	save_file[SZ_FNAME]		# File to save text output
char	save_temp[SZ_FNAME]		# File to save temp text output
char	ans[2*SZ_LINE]			# String contains answers
char	ansn[2*SZ_LINE, 4]		# String for multiple answer lines

int	newgraph, newimage, options[NOPTIONS]
pointer	gp, gt, im, pix, ids, sp
bool	wave_scl, fnu

pointer	gopen(), gt_init()
int	clgcur(), imtopen(), imtgetim(), imaccess(), access()
real	clgetr()
bool	streq()
errchk	getimage, fun_do

begin
	# Get task parameters.

	call clgstr ("images", ans, SZ_LINE)
	list = imtopen (ans)
	call clgstr ("save_file", save_file, SZ_FNAME)
	call clgstr ("options", ans, SZ_LINE)
	call xt_gids (ans, OPTIONS, options, NOPTIONS)
	call mktemp ("splot", save_temp, SZ_FNAME)

	# Allocate space for User area
	call smark (sp)
	call salloc (ids, LEN_IDS, TY_STRUCT)
	call salloc (POINT(ids), MAX_NCOEFF, TY_REAL)
	pix = NULL

	# Initialize graph format
	gt = gt_init()
	call gt_setr (gt, GTXMIN, clgetr ("xmin"))
	call gt_setr (gt, GTXMAX, clgetr ("xmax"))
	call gt_setr (gt, GTYMIN, clgetr ("ymin"))
	call gt_setr (gt, GTYMAX, clgetr ("ymax"))
	call gt_sets (gt, GTTYPE, "line")

	if (options[ZERO] == YES)
	    call gt_setr (gt, GTYMIN, 0.)

	while (imtgetim (list, image, SZ_FNAME) != EOF) {

	    # Initialize to plot a wavelength scale
	    wave_scl = true

	    # Open image and get pixels
	    if (imaccess (image, READ_ONLY) == NO) {
		call eprintf ("Can't get image %s\n")
		    call pargstr (image)
		next
	    }
	    call getimage (image, nline, wave_scl, im, ids, gt, pix, npts,
		x1, x2, dx)
	    newimage = YES

	    # Open plotter, eliminate y-axis minor ticks, and enter cursor
	    # loop with 'r' redraw.
	    call clgstr ("graphics", command, SZ_FNAME)
	    gp = gopen (command, NEW_FILE, STDGRAPH)
#	    call gseti (gp, G_YNMINOR, 0)
	    key = 'r'
	    repeat {
	        switch (key) {
	        case ':':
		    if (command[1] == '/')
		        call gt_colon (command, gp, gt, newgraph)
		    else if (command[1] == 's') {
			if (access (save_temp, READ_ONLY, TEXT_FILE) == YES)
		            call gpagefile (gp, save_temp, "splot data")
			else
			    call printf ("No measurements\n")
		    } else
		        call printf ("\07\n")

	        case 'a': # Autoexpand
		    call auto_exp (gp, gt, key, wx, Memr[pix], npts, x1, x2)

	        case 'b': # Set base to 0.0 by rescaling
		    # Cheap method --> Set first pixel value to 0.0
		    call gt_setr (gt, GTYMIN, 0.)
		    newgraph = options[AUTO]

	        case 'c':
		    call printf ("x,y: %10.3f %10.4g\n")
		        call pargr (wx)
		        call pargr (wy)
		    call flush (STDOUT)
    
	        case 'd': # De-blend a group of lines
		    call deblend (gp, x1, x2, dx, wx, wy, Memr[pix], ansn,
			nans)
		    do i = 1, nans {
		        call ans_save (image, nline, IM_TITLE(im), ansn[1,i],
			    save_file, newimage)
		        call ans_save (image, nline, IM_TITLE(im), ansn[1,i],
			    save_temp, newimage)
			newimage = NO
		    }

	        case 'e': # Equivalent width
		    call eqwidth (gp, wx, wy, x1, x2, dx, Memr[pix], ans)
		    call ans_save (image, nline, IM_TITLE(im), ans, save_file,
			newimage)
		    call ans_save (image, nline, IM_TITLE(im), ans, save_temp,
			newimage)
		    newimage = NO

	        case 'k': # Equivalent width -- C. Pilachowski style
		    # Continuum at 1.
		    call eqwidth_cp (gp, npts, wx, 1., wy, x1, x2, dx,
			Memr[pix], key, ans)
		    call ans_save (image, nline, IM_TITLE(im), ans, save_file,
			newimage)
		    call ans_save (image, nline, IM_TITLE(im), ans, save_temp,
			newimage)
		    newimage = NO

	        case 'v': # Equivalent width -- C. Pilachowski style
		    # Continuum at cursor, width determined at half flux.
		    call eqwidth_cp (gp, npts, wx, wy, INDEF, x1, x2, dx,
			Memr[pix], key, ans)
		    call ans_save (image, nline, IM_TITLE(im), ans, save_file,
			newimage)
		    call ans_save (image, nline, IM_TITLE(im), ans, save_temp,
			newimage)
		    newimage = NO

	        case 'h': # As above but only left or right half of line
		    repeat {
			switch (key) {
			case 'a', 'b':
		            call eqwidth_cp (gp, npts, wx, wy, INDEF, x1, x2,
				dx, Memr[pix], key, ans)
			    break
			case 'l', 'r':
		            call eqwidth_cp (gp, npts, wx, 1., wy, x1, x2, dx,
			        Memr[pix], key, ans)
			    break
			default:
		    	    call printf ("Set cursor and type a, b, l, or r:")
			}
		    } until (clgcur ("cursor", wx, wy, wc, key, command,
			SZ_FNAME) == EOF)
		    call ans_save (image, nline, IM_TITLE(im), ans, save_file,
			newimage)
		    call ans_save (image, nline, IM_TITLE(im), ans, save_temp,
			newimage)
		    newimage = NO

	        case 'g', 'o': # Get new image to plot
		    call clgstr ("next_image", command, SZ_FNAME)
		    if (streq (image, command)) {
			call imunmap (im)
		    } else if (imaccess (command, READ_ONLY) == YES) {
			call imunmap (im)
			newimage = YES
		    } else {
			call eprintf ("Can't get %s\n")
			    call pargstr (command)
			next
		    }

		    i = nline
		    call strcpy (command, image, SZ_FNAME)
		    call getimage (image, nline, wave_scl, im, ids, gt, pix,
			npts, x1, x2, dx)
		    if (nline != i)
			newimage = YES

		    switch (key) {
		    case 'g':
		        newgraph = YES
		    case 'o':
		        call printf ("Overplotting: %s")
			    call pargstr (image)
			if (nline > 0) {
			    call printf ("(%d)")
				call pargi (nline)
			}
		        call flush (STDOUT)
			call replot (gp, gt, Memr[pix], npts, x1, x2, NO)
		    }

	        case 'w': # Window the graph
		    call gt_window (gt, gp, "cursor", newgraph)
		    if (newgraph == YES)
			newgraph = options[AUTO]
		
	        case 'l': # Convert to f-lambda - issue warning if not a
			  # calibrated image
		    if (CA_FLAG(ids) != 0)
		        call eprintf (
			    "Warning: (>flam) spectrum not calibrated\n")

		    call conflam (Memr[pix], W0(ids), WPC(ids), npts)
		    newgraph = options[AUTO]

	        case 'f': # Function operators
		    call fun_help ()
		    while (clgcur ("cursor", wx, wy, wc, key, command, 
		        SZ_FNAME) != EOF) {
			switch (key) {
			case '?':
			    call fun_help ()
			case 'q':
			    break
			case 'I':
			    call fatal (0, "Interrupt")
			default:
			    iferr {
		                call fun_do (key, Memr[pix], npts)
		                if (options[AUTO] == YES)
			            call replot (gp, gt, Memr[pix], npts, x1,
					x2, YES)
		                call fun_help ()
			    } then
			        call erract (EA_WARN)
			}
		    }
		    call printf ("\n")

	        case 'm': # Signal-to-noise
		    call avgsnr (gp, wx, wy, x1, x2, dx, Memr[pix])

	        case 'n': # Convert to f-nu
		    if (CA_FLAG(ids) != 0)
		        call eprintf (
			    "Warning: (>fnu) spectrum not calibrated\n")

		    call confnu (Memr[pix], W0(ids), WPC(ids), npts)
		    newgraph = options[AUTO]

		case 'q':
		    break

	        case 'p': # Convert to wavelength x-scale
		    if (!IS_INDEF (WPC(ids))) {
			x1 = W0(ids)
			dx = WPC(ids)
			x2 = x1 + (npts-1) * dx
			call clputr ("wstart", x1)
			call clputr ("wend", x2)
		    }

		    x1 = clgetr ("wstart")
		    x2 = clgetr ("wend")

		    # User may enter a dispersion rather than an ending
		    # wavelength. Assume a dispersion entry if less than
		    # 1/100 of the initial wavelength

		    if (abs(x2) < abs(x1/100.0)) {
			dx = x2
			x2 = x1 + (npts-1) * dx
		    } else
		        dx = (x2-x1) / (npts-1)

		    W0(ids) = x1
		    WPC(ids) = dx
		    DC_FLAG(ids) = 0
		    wave_scl = true

		    newgraph = options[AUTO]

	        case 'r': # Replot
		    newgraph = YES

	        case 's': # Smooth
		    call smooth (gp, Memr[pix], npts, ids)
		    newgraph = options[AUTO]

	        case 't': # FlaTTen spectrum
		    call flatten (Memr[pix], npts)
		    newgraph = options[AUTO]

	        case 'u': # Set User coordinates - mark 2 lines
		    call user_coord (ids, wave_scl, npts, wx, x1, x2, dx)
		    newgraph = options[AUTO]

	        case 'i': # Write image spectrum out
		    call wrspect (image, Memr[pix], npts, ids, im)

	        case 'j': # Fudge (fix) a data point
		    call fudgept (gp, Memr[pix], npts, x1, x2, dx, wx, wy)

	        case 'x': # Fudge eXtended over a line
		    call fudgex (gp, Memr[pix], npts, x1, x2, dx, wx, wy)

	        case 'y': # Over plot standard star data
		    # Estimate data is fnu or flambda: cutoff around dexp[-20]
		    fnu = false
		    call aavgr (Memr[pix], npts, avg_pix, sigma_pix)
		    if (log10 (avg_pix) < -19.5)
		        fnu = true
		    call plot_std (gp, x1, dx, npts, fnu)
		    call printf ("\n")

	        case 'z': # Zoom x region to larger range
		    call auto_exp (gp, gt, key, wx, Memr[pix], npts, x1, x2)

	        case '-': # Subtract deblend fit
		    call subblend (gp, Memr[pix], x1, x2, dx, wx, wy)
		    #newgraph = options[AUTO]

	        case '.': # Slide upward
		    call auto_exp (gp, gt, key, wx, Memr[pix], npts, x1, x2)

	        case ',': # Slide downward
		    call auto_exp (gp, gt, key, wx, Memr[pix], npts, x1, x2)

	        case '$': # Convert to channel scale
		    x1 = 1.0
		    dx = 1.0
		    x2 = x1 + (npts-1) * dx
		    wave_scl = false
		    newgraph = options[AUTO]

	        case '/': # Help on status line
		    call sts_help

	        case '?': # Help screen
		    call gpagefile (gp, KEY, PROMPT)

		case 'I': # Interrupt
		    call fatal (0, "Interrupt")

	        default: # Default = 'c'
		    call pixind (x1, x2, dx, wx, i)
		    i = max (1, min (npts, i))
		    call printf ("x,y,z(x): %10.3f %10.4g %10.4g\n")
		        call pargr (wx)
		        call pargr (wy)
		        call pargr (Memr[pix+i-1])
	        }

	        if (newgraph == YES) {
		    call replot (gp, gt, Memr[pix], npts, x1, x2, YES)
		    newgraph = NO
	        }
	    } until (clgcur ("cursor",wx,wy,wc,key,command,SZ_FNAME) == EOF)
	    call gclose (gp)
	    if (im != ERR)
	        call imunmap (im)
	}

	iferr (call delete (save_temp))
	    ;
	call mfree (pix, TY_REAL)
	call sfree  (sp)
	call gt_free (gt)
	call imtclose (list)
end
