include	<ctype.h>
include	<imhdr.h>
include	<error.h>
include	<gset.h>
include	<mach.h>
include	<pkg/gtools.h>
include	<smw.h>
include	<units.h>
include	"specplot.h"

# Define the help information.
define	HELP		"noao$onedspec/specplot.key"
define	PROMPT		"specplot options"

 
# T_SPECPLOT -- Plot multiple spectra in a variety of formats and layouts.
# The spectra may be individually scaled and offset in intensity, shifted
# and scaled in wavelength, and plotted in uniform steps.  The plotting
# type may be symbols or lines.  The spectra may be labeled.  Each spectrum
# is read into memory in a structre defined in "specplot.h".  An array
# of structures is then manipulated.  Each line of two dimensional images
# are treated as separate spectra.

procedure t_specplot ()

pointer	list			# List of input spectra
real	step			# Initial separation step
int	labels			# Labeling mode
real	fraction		# Fraction of minimum step
bool	yscale			# Draw y scale?

bool	wscale
int	i, j, n, fd, nspec, wcs, key, redraw
real	wx, wy, wx1, wy1, wx2, wy2
pointer	stack, units, cmd, sp, sh, spsave, sps, gp, gt

bool	clgetb()
int	clgwrd(), clgcur()
int	open(), imtgetim(), getline(), scan(), nscan()
int	stridxs(), nowhite(), btoi(), gt_geti()
real	clgetr()
pointer	sp_nearest(), imtopenp(), gopen(), gt_init()
errchk	sp_gdata, un_changer

define	nospec_	99

begin
	call smark (stack)
	call salloc (units, SZ_LINE, TY_CHAR)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	call calloc (sps, 100, TY_POINTER)
	spsave = NULL

	# Read the input spectrum list into an array of structures.
	i = 0
	nspec = 0
	list = imtopenp ("spectra")
	call clgstr ("units", Memc[units], SZ_LINE)
	if (nowhite (Memc[units], Memc[cmd], SZ_LINE) == 0)
	    call strcpy ("display", Memc[units], SZ_LINE)
	while (imtgetim (list, Memc[cmd], SZ_FNAME) != EOF) {
	    iferr (call sp_gdata (Memc[cmd], Memc[units], i, sps, nspec))
		call erract (EA_WARN)
	}
	call imtclose (list)


	# Set the layout of the spectra.
	step = clgetr ("step")
	fraction = clgetr ("fraction")
	if (clgetb ("autolayout")) {
	    if (clgetb ("autoscale"))
	        call sp_autolayout (Memi[sps], nspec, true, fraction, step)
	    else
	        call sp_autolayout (Memi[sps], nspec, false, fraction, step)
	}
	call sp_scale (Memi[sps], nspec, step)

	# Get optional user labels from a file and set the label type.
	call clgstr ("ulabels", Memc[cmd], SZ_FNAME)
	ifnoerr (fd = open (Memc[cmd], READ_ONLY, TEXT_FILE)) {
	    do i = 1, nspec {
		sp = Memi[sps+i-1]
		if (getline (fd, Memc[cmd]) != EOF)
		    call strcpy (Memc[cmd], SP_ULABEL(sp), SP_SZULABEL)
		else
		    SP_ULABEL(sp) = EOS
		j = stridxs ("\n", SP_ULABEL(sp))
		if (j > 0)
		    call strcpy (SP_ULABEL(sp), SP_ULABEL(sp), j-1)
	    }
	    call close (fd)
	}
	labels = clgwrd ("labels", Memc[cmd], SZ_FNAME, LABELS)
	call sp_labels (Memi[sps], nspec, labels)

	# Initialize the graphics
	call clgstr ("graphics", Memc[cmd], SZ_FNAME)
	gp = gopen (Memc[cmd], NEW_FILE, STDGRAPH)

	gt = gt_init ()
	call gt_seti (gt, GTSYSID, btoi (clgetb ("sysid")))
	call clgstr ("title", Memc[cmd], SZ_LINE)
	call gt_sets (gt, GTTITLE, Memc[cmd])
	call clgstr ("xlabel", Memc[cmd], SZ_LINE)
	if (Memc[cmd] != EOS) {
	    call gt_sets (gt, GTXLABEL, Memc[cmd])
	    call gt_sets (gt, GTXUNITS, "")
	} else if (nspec > 0) {
	    if (UN_LABEL(UN(SP_SH(Memi[sps]))) != EOS) {
		call gt_sets (gt, GTXLABEL, UN_LABEL(UN(SP_SH(Memi[sps]))))
		call gt_sets (gt, GTXUNITS, UN_UNITS(UN(SP_SH(Memi[sps]))))
	    } else {
		call gt_sets (gt, GTXLABEL, LABEL(SP_SH(Memi[sps])))
		call gt_sets (gt, GTXUNITS, UNITS(SP_SH(Memi[sps])))
	    }
	}
	call clgstr ("ylabel", Memc[cmd], SZ_LINE)
	call gt_sets (gt, GTYLABEL, Memc[cmd])
	wx = clgetr ("xmin")
	call gt_setr (gt, GTXMIN, wx)
	wx = clgetr ("xmax")
	call gt_setr (gt, GTXMAX, wx)
	wx = clgetr ("ymin")
	call gt_setr (gt, GTYMIN, wx)
	wx = clgetr ("ymax")
	call gt_setr (gt, GTYMAX, wx)
	wscale = true
	yscale = clgetb ("yscale")
	#if (!scale)
	#    call gseti (gp, G_YDRAWTICKS, NO)

	# Draw the graph on the first pass and then read the cursor.
	key = 'r'
	repeat {
	    switch (key) {
	    case '?': # Page help summary
		call gpagefile (gp, HELP, PROMPT)
	    case ':': # Process colon commands
		if (Memc[cmd] == '/')
		    call gt_colon (Memc[cmd], gp, gt, redraw)
		else {
		    i = sp_nearest (gp, wx, wy, key, Memc[cmd], Memi[sps],
			nspec)
		    call sp_colon (Memc[cmd], gp, gt, Memi[sps], nspec,
			Memc[units], labels, i, step, fraction, redraw)
		    if (nspec == 0) {
			redraw = NO
			goto nospec_
		    }
		}
	    case 'a', 'i': # Append or insert a new spectrum
		i = sp_nearest (gp, wx, wy, key, Memc[cmd], Memi[sps], nspec)
		if (key == 'i')
		    i = max (0, i - 1)
		call printf ("Spectrum: ")
		call flush (STDOUT)
		if (scan() != EOF) {
		    call gargwrd (Memc[cmd], SZ_LINE)
		    if (nscan() == 1) {
			iferr {
	    		    call sp_gdata (Memc[cmd], Memc[units],
				i, sps, nspec)
	    	    	    call sp_labels (Memi[sps], nspec, labels)
	    	    	    call sp_scale (Memi[sps], nspec, step)
		    	    redraw = YES
			} then
			    call erract (EA_WARN)
		    }
		}
	    case 'd': # Delete a spectrum
		if (nspec == 0)
		    goto nospec_

		i = sp_nearest (gp, wx, wy, key, Memc[cmd], Memi[sps], nspec)
		sp = Memi[sps+i-1]
	    	call sp_ptype (SP_PTYPE(sp), SP_COLOR(sp), YES, gp, gt)
	    	call gt_plot (gp, gt, SP_X(sp), SP_Y(sp), SP_NPTS(sp))
		call gline (gp, SP_X(sp), SP_Y(sp), SP_X(sp), SP_Y(SP))
		call sp_delete (i, sps, nspec)
	    	call sp_labels (Memi[sps], nspec, labels)
	    	call sp_scale (Memi[sps], nspec, step)
		if (spsave != NULL)
		    call sp_free (spsave)
		spsave = sp
#		redraw = YES
	    case 'e': # Undelete a spectrum
		if (spsave != NULL) {
		    i = sp_nearest (gp, wx, wy, key, Memc[cmd], Memi[sps],
			nspec)
		    i = max (0, i - 1)
		    call sp_add (spsave, i, sps, nspec)
		    call sp_labels (Memi[sps], nspec, labels)
		    call sp_scale (Memi[sps], nspec, step)
		    spsave = NULL
		    redraw = YES
		}
	    case 'f': # Toggle wavelength scale
		if (wscale) {
		    call gt_sets (gt, GTXLABEL, "Pixels")
		    call gt_sets (gt, GTXUNITS, "")
		    wscale = false
		} else {
		    if (nspec > 0) {
			sp = Memi[sps]
			sh = SP_SH(sp) 
			call gt_sets (gt, GTXLABEL, UN_LABEL(UN(sh)))
			call gt_sets (gt, GTXUNITS, UN_UNITS(UN(sh)))
		    }
		    wscale = true
		}
		redraw = YES
	    case 'l', 'p': # Mark label position and enter label.
		if (nspec == 0)
		    goto nospec_

		i = sp_nearest (gp, wx, wy, key, Memc[cmd], Memi[sps], nspec)
		sp = Memi[sps+i-1]
		call printf (
		    "Spectrum %d:  Mark position for label ('q' to cancel)")
		    call pargi (SP_INDEX(sp))
		i = clgcur ("cursor", wx, wy, wcs, j, Memc[cmd], SZ_LINE)
		if (j != 'q') {
		    call ggwind (gp, wx1, wx2, wy1, wy2)
		    wx2 = wx2 - wx1
		    wy2 = wy2 - wy1
		    SP_XLPOS(sp) = (wx - wx1) / wx2
		    SP_YLPOS(sp) = (wy - SP_MEAN(sp)) / wy2

		    if (key == 'l') {
		        call printf ("Spectrum %d: Label = ")
		            call pargi (SP_INDEX(sp))
		        call flush (STDOUT)
		        if (scan() != EOF) {
		            call gargstr (SP_ULABEL(sp), SP_SZULABEL)
			    j = stridxs ("\n", SP_ULABEL(sp))
			    if (j > 0)
			        call strcpy (SP_ULABEL(sp), SP_ULABEL(sp), j-1)
			    call strcpy (SP_ULABEL(sp), SP_LABEL(sp),
				SP_SZLABEL)
		        }
		    }
		    call gtext (gp, wx, wy, SP_LABEL(sp), "")
		}
		call printf ("\n")
	    case 'o': # Reorder the spectra to eliminate gaps.
		if (nspec == 0)
		    goto nospec_

		do i = 1, nspec {
		    sp = Memi[sps+i-1]
		    if (SP_INDEX(sp) != i) {
		        SP_INDEX (sp) = i
			redraw = YES
		    }
		}
		if (redraw == YES) {
	            call sp_labels (Memi[sps], nspec, labels)
	            call sp_scale (Memi[sps], nspec, step)
		}
	    case 'q', 'I': # Quit or interrupt
		break
	    case 'r': # Redraw the current graph
		redraw = YES
	    case 's': # Shift the spectrum nearest the cursor
		if (nspec == 0)
		    goto nospec_

		i = sp_nearest (gp, wx, wy, key, Memc[cmd], Memi[sps], nspec)
		sp = Memi[sps+i-1]
		call printf ( "Shift spectrum %d: (q, r, s, t, x, y, z)")
		    call pargi (SP_INDEX(sp))
		while (clgcur ("cursor", wx1, wy1, wcs, key, Memc[cmd],
		    SZ_LINE) != EOF) {
		    switch (key) {
		    case 's':
			if (wy != SP_OFFSET(sp)) {
	    	            call sp_ptype (SP_PTYPE(sp), SP_COLOR(sp),
				YES, gp, gt)
	    		    call gt_plot (gp, gt, SP_X(sp), SP_Y(sp),
				SP_NPTS(sp))
			    call gline (gp, SP_X(sp), SP_Y(sp), SP_X(sp),
				SP_Y(sp))
			    SP_SCALE(sp) = SP_SCALE(sp) *
				(wy1 - SP_OFFSET(sp)) / (wy - SP_OFFSET(sp))
		            call sp_scale (sp, 1, step)
	    	            call sp_ptype (SP_PTYPE(sp), SP_COLOR(sp),
				NO, gp, gt)
	    		    call gt_plot (gp, gt, SP_X(sp), SP_Y(sp),
				SP_NPTS(sp))
			    wy = wy1
			}
		    case 't':
			if (wy != SP_OFFSET(sp)) {
	    	            call sp_ptype (SP_PTYPE(sp), SP_COLOR(sp),
				YES, gp, gt)
	    		    call gt_plot (gp, gt, SP_X(sp), SP_Y(sp),
				SP_NPTS(sp))
			    call gline (gp, SP_X(sp), SP_Y(sp), SP_X(sp),
				SP_Y(sp))
			    if (UN_CLASS(UN(SP_SH(sp))) == UN_VEL)
				SP_XOFFSET(sp) = SP_XOFFSET(sp) + wx1 - wx
			    else
				SP_XSCALE(sp) = SP_XSCALE(sp) * wx1 / wx
			    SP_SCALE(sp) = SP_SCALE(sp) *
				(wy1 - SP_OFFSET(sp)) / (wy - SP_OFFSET(sp))
		            call sp_scale (sp, 1, step)
	    	            call sp_ptype (SP_PTYPE(sp), SP_COLOR(sp),
				NO, gp, gt)
	    		    call gt_plot (gp, gt, SP_X(sp), SP_Y(sp),
				SP_NPTS(sp))
			    wx = wx1
			    wy = wy1
			}
		    case 'x':
	    	        call sp_ptype (SP_PTYPE(sp), SP_COLOR(sp),
			    YES, gp, gt)
	    		call gt_plot (gp, gt, SP_X(sp), SP_Y(sp), SP_NPTS(sp))
			call gline (gp, SP_X(sp), SP_Y(sp), SP_X(sp), SP_Y(SP))
			if (UN_CLASS(UN(SP_SH(sp))) == UN_VEL)
			    SP_XOFFSET(sp) = SP_XOFFSET(sp) + wx1 - wx
			else
			    SP_XSCALE(sp) = SP_XSCALE(sp) * wx1 / wx
		        call sp_scale (sp, 1, step)
	    	        call sp_ptype (SP_PTYPE(sp), SP_COLOR(sp),
			    NO, gp, gt)
	    		call gt_plot (gp, gt, SP_X(sp), SP_Y(sp), SP_NPTS(sp))
			wx = wx1
		    case 'y':
	    	        call sp_ptype (SP_PTYPE(sp), SP_COLOR(sp),
			    YES, gp, gt)
	    		call gt_plot (gp, gt, SP_X(sp), SP_Y(sp), SP_NPTS(sp))
			call gline (gp, SP_X(sp), SP_Y(sp), SP_X(sp), SP_Y(SP))
			SP_OFFSET(sp) = SP_OFFSET(sp) + wy1 - wy
		        call sp_scale (sp, 1, step)
	    	        call sp_ptype (SP_PTYPE(sp), SP_COLOR(sp),
			    NO, gp, gt)
	    		call gt_plot (gp, gt, SP_X(sp), SP_Y(sp), SP_NPTS(sp))
			wy = wy1
		    case 'z':
	    	        call sp_ptype (SP_PTYPE(sp), SP_COLOR(sp),
			    YES, gp, gt)
	    		call gt_plot (gp, gt, SP_X(sp), SP_Y(sp), SP_NPTS(sp))
			call gline (gp, SP_X(sp), SP_Y(sp), SP_X(sp), SP_Y(SP))
			if (UN_CLASS(UN(SP_SH(sp))) == UN_VEL)
			    SP_XOFFSET(sp) = SP_XOFFSET(sp) + wx1 - wx
			else
			    SP_XSCALE(sp) = SP_XSCALE(sp) * wx1 / wx
			SP_OFFSET(sp) = SP_OFFSET(sp) + wy1 - wy
		        call sp_scale (sp, 1, step)
	    	        call sp_ptype (SP_PTYPE(sp), SP_COLOR(sp),
			    NO, gp, gt)
	    		call gt_plot (gp, gt, SP_X(sp), SP_Y(sp), SP_NPTS(sp))
			wx = wx1
			wy = wy1
		    case 'r':
			if (gt_geti (gt, GTSYSID) == YES) {
			    call sprintf (Memc[cmd], SZ_LINE,
				"Separation step = %g")
			        call pargr (step)
			    call gt_sets (gt, GTPARAMS, Memc[cmd])
			} else
			    call gt_sets (gt, GTPARAMS, "")
			call sp_plot (gp, gt, Memi[sps], nspec, wscale, yscale)
		    case 'q':
			break
		    }
		    call printf ( "Shift spectrum %d: (q, r, s, t, x, y, z)")
		        call pargi (SP_INDEX(sp))
		}
		call printf ("\n")
	    case 't': # Set a wavelength scale using the cursor.
		if (nspec == 0)
		    goto nospec_

		i = sp_nearest (gp, wx, wy, key, Memc[cmd], Memi[sps], nspec)
		sp = Memi[sps+i-1]
		call printf ("X coordinate (%g): ")
		    call pargr (wx)
		    call flush (STDOUT)
		if (scan() != EOF) {
		    call gargr (wy)
		    if (nscan() == 0)
			wy = wx
		} else
		    wy = wx
		call printf ("Mark another position")
		i = clgcur ("cursor", wx1, wy1, wcs, key, Memc[cmd], SZ_LINE)
		call printf ("X coordinate (%g): ")
		    call pargr (wx1)
		    call flush (STDOUT)
		if (scan() != EOF) {
		    call gargr (wy1)
		    if (nscan() == 0)
			wy1 = wx1
		} else
		    wy1 = wx1
		if (wx != wx1) {
		    n = SP_NPTS(sp) - 1
		    sh = SP_PX(sp) - 1
		    if (SP_WPC(sp) > 0.) {
			for (i=1; i<n && wx<Memr[sh+i]; i=i+1)
			    ;
			for (j=1; j<n && wx1<Memr[sh+j]; j=j+1)
			    ;
		    } else {
			for (i=1; i>n && wx>Memr[sh+i]; i=i+1)
			    ;
			for (j=1; j>n && wx1>Memr[sh+j]; j=j+1)
			    ;
		    }
		    wx = i + (wx - Memr[sh+i]) / (Memr[sh+i+1] - Memr[sh+i])
		    wx1 = j + (wx1 - Memr[sh+j]) / (Memr[sh+j+1] - Memr[sh+j])
		    SP_WPC(sp) = (wy - wy1) / (wx - wx1)
		    SP_W0(sp) = wy - SP_WPC(sp) * (wx - 1)
		    call sp_linear (sp)
		    call sp_scale (sp, 1, step)
		    redraw = YES
		}
	    case 'u': # Set a wavelength point using the cursor.
		if (nspec == 0)
		    goto nospec_

		i = sp_nearest (gp, wx, wy, key, Memc[cmd], Memi[sps], nspec)
		sp = Memi[sps+i-1]
		call printf ("X coordinate (%g): ")
		    call pargr (wx)
		    call flush (STDOUT)
		if (scan() != EOF) {
		    call gargr (wx1)
		    if (nscan() == 1) {
		        SP_XOFFSET(sp) = SP_XOFFSET(sp) + wx1 - wx
			call sp_scale (sp, 1, step)
			redraw = YES
		    }
		}
	    case 'v': # Change to velocity scale
		if (nspec == 0)
		    goto nospec_

		iferr {
		    do i = 1, nspec {
			sp = Memi[sps+i-1]
			sh = SP_SH(sp) 
			if (i == 1) {
			    call un_changer (UN(sh), "angstroms", wx, 1, NO)
			    call sprintf (Memc[units], SZ_LINE,
				"km/s %g angstroms")
				call pargr (wx)
			    call un_changer (UN(sh), Memc[units], Memr[SX(sh)],
				SN(sh), YES)
			    call gt_sets (gt, GTXLABEL, UN_LABEL(UN(sh)))
			    call gt_sets (gt, GTXUNITS, UN_UNITS(UN(sh)))
			    redraw = YES
			} else
			    call un_changer (UN(sh), Memc[units], Memr[SX(sh)],
				SN(sh), YES)
			SP_W0(sp) = Memr[SX(sh)]
			SP_WPC(sp) = (Memr[SX(sh)+SN(sh)-1] - Memr[SX(sh)]) /
			    (SN(sh) - 1)
			SP_XSCALE(sp) = 1.
			SP_XOFFSET(sp) = 0.
			call sp_scale (sp, 1, step)
		    }
		} then
		    call erract (EA_WARN)
	    case 'w': # Window the graph
		call gt_window (gt, gp, "cursor", redraw)
	    case 'x': # No layout
		if (nspec == 0)
		    goto nospec_

		do i = 1, nspec {
		    sp = Memi[sps+i-1]
		    SP_SCALE(sp) = 1.
		    SP_OFFSET(sp) = 0.
		}
	        call sp_scale (Memi[sps], nspec, step)
		redraw = YES
	    case 'y': # Layout the spectra offsets to common mean
		if (nspec == 0)
		    goto nospec_

	        call sp_autolayout (Memi[sps], nspec, false, fraction, step)
	        call sp_scale (Memi[sps], nspec, step)
		redraw = YES
	    case 'z': # Layout the spectra scaled to common mean
		if (nspec == 0)
		    goto nospec_

	        call sp_autolayout (Memi[sps], nspec, true, fraction, step)
	        call sp_scale (Memi[sps], nspec, step)
		redraw = YES
	    default:
		call printf ("\007")
	    }

	    # Redraw the graph as needed.
	    if (redraw == YES) {
		if (gt_geti (gt, GTSYSID) == YES) {
		    call sprintf (Memc[cmd], SZ_LINE, "Separation step = %g")
		        call pargr (step)
		    call gt_sets (gt, GTPARAMS, Memc[cmd])
		} else
		    call gt_sets (gt, GTPARAMS, "")
		call sp_plot (gp, gt, Memi[sps], nspec, wscale, yscale)
		redraw = NO
	    }
nospec_
	if (nspec == 0)
		call printf ("No spectra defined\007")

	} until (clgcur ("cursor", wx, wy, wcs, key, Memc[cmd], SZ_LINE) == EOF)

	
	call clgstr ("logfile", Memc[cmd], SZ_LINE)
	if (nowhite (Memc[cmd], Memc[cmd], SZ_LINE) > 0)
	    iferr (call sp_vshow (Memc[cmd], NULL, Memi[sps], nspec, step))
		call erract (EA_WARN)

	# Close the graphics device and free memory.
	call gclose (gp)
	call gt_free (gt)

	if (nspec > 0) {
	    do i = 1, nspec
		call sp_free (Memi[sps+i-1])
	}
	if (spsave != NULL)
	    call sp_free (spsave)
	call mfree (sps, TY_POINTER)
	call sfree (stack)
end


# SP_SCALE -- Scale the spectra.  This uses the wavelength scale and intensity
# scale parameters defined for each spectrum and adds the intensity offset. 

procedure sp_scale (sps, nspec, step)

pointer	sps[ARB]	# Spectrum structures
int	nspec		# Number of spectra
real	step		# Final step

int	i, npts
real	scale, offset
pointer	sp, sh

begin
	do i = 1, nspec {
	    sp = sps[i]
	    sh = SP_SH(sp)
	    npts = SP_NPTS(sp)

	    scale = SP_XSCALE(sp)
	    offset = SP_XOFFSET(sp)
	    call altmr (Memr[SX(sh)], SP_X(sp), npts, scale, offset)

	    scale = SP_SCALE(sp)
	    offset = SP_OFFSET(sp) + (SP_INDEX(sp) - 1) * step
	    call altmr (Memr[SY(sh)], SP_Y(sp), npts, scale, offset)

	    SP_MEAN(sp) = SP_OMEAN(sp) * scale + offset
	    SP_MIN(sp) = SP_OMIN(sp) * scale + offset
	    SP_MAX(sp) = SP_OMAX(sp) * scale + offset
	}
end


# SP_AUTOLAYOUT -- Apply an automatic layout algorithm in which the spectra
# are scaled or offset to a common mean and a separation step is computed
# to provide a specified degree of overlap between the nearest spectra.

procedure sp_autolayout (sps, nspec, autoscale, fraction, step)

pointer	sps[ARB]	# Spectrum structures
int	nspec		# Number of spectra
bool	autoscale	# Scale spectra to common mean?
real	fraction	# Fraction to adjust step
real	step		# Final step

int	i
real	a, b, scale, offset
pointer	sp

begin
	if (nspec < 2)
	    return

	# Scale to the lowest indexed spectrum (usually 1).
	sp = sps[1]
	scale = SP_SCALE(sp)
	offset = SP_OFFSET(sp)
	a = SP_OMEAN(sp)

	# If desired use a multiplicative scaling to a common mean.
	# If the mean is <= 0 then use offset to common mean.

	if (autoscale) {
	    do i = 2, nspec {
	        sp = sps[i]
	        if (a * SP_OMEAN(sp) > 0.) {
		    SP_SCALE(sp) = a / SP_OMEAN(sp) * scale
		    SP_OFFSET(sp) = offset
	        } else {
		    SP_SCALE(sp) = scale
		    SP_OFFSET(sp) = (a - SP_OMEAN(sp)) * scale + offset
	        }
	    }

	# Otherwise use an offset scaling to a common mean.
	} else {
	    do i = 2, nspec {
	        sp = sps[i]
		SP_SCALE(sp) = scale
		SP_OFFSET(sp) = (a - SP_OMEAN(sp)) * scale + offset
	    }
	}

	# Compute the minimum step which just separates the maximum of
	# one spectrum from the minimum of the next spectrum.  A degree
	# of overlap can be set using the fraction parameter.

	step = -MAX_REAL
	do i = 2, nspec {
	    sp = sps[i-1]
	    a = SP_OMAX(sp) * SP_SCALE(sp) + SP_OFFSET(sp)
	    sp = sps[i]
	    b = SP_OMIN(sp) * SP_SCALE(sp) + SP_OFFSET(sp)
	    step = max (step, a - b)
	}
	step = fraction * step
end


# SP_PLOT -- Determine the range of all the data and then make a plot with
# specified labels.  The GTOOLS procedures are used to allow user adjustment.

procedure sp_plot (gp, gt, sps, nspec, wscale, yscale)

pointer	gp		# GIO pointer
pointer	gt		# GTOOLS pointer
pointer	sps[ARB]	# Spectrum structures
int	nspec		# Number of spectra
bool	wscale		# Draw in world coordinates?
bool	yscale		# Draw Y scale?

int	i, n
real	x, y, xmin, xmax, ymin, ymax
pointer	sp, pix

begin
	# Set the default limits from the data.
	xmin = MAX_REAL
	xmax = -MAX_REAL
	ymin = MAX_REAL
	ymax = -MAX_REAL
	n = 0
	do i = 1, nspec {
	    sp = sps[i]
	    if (wscale) {
		xmin = min (xmin, SP_X(sp), Memr[SP_PX(sp)+SP_NPTS(sp)-1])
		xmax = max (xmax, SP_X(sp), Memr[SP_PX(sp)+SP_NPTS(sp)-1])
	    } else {
		n = max (n, SP_NPTS(sp))
		xmin = 1
		xmax = n
	    }
	    ymin = min (ymin, SP_MIN(sp))
	    ymax = max (ymax, SP_MAX(sp))
	}

	if (xmin > xmax) {
	    xmin = 0.
	    xmax = 1.
	}
	if (ymin > ymax) {
	    ymin = 0.
	    ymax = 1.
	}

	# Draw the axes with GTOOLS limits override.
	#call gframe (gp)
	call gclear (gp)
	if (!yscale)
	    call gseti (gp, G_YDRAWTICKS, NO)
	call gswind (gp, xmin, xmax, ymin, ymax)
	call gt_swind (gp, gt)
	call gt_labax (gp, gt)

	# The label positions are based on the limits of the graph.
	call ggwind (gp, xmin, xmax, ymin, ymax)
	xmax = xmax - xmin
	ymax = ymax - ymin

	if (!wscale) {
	    call malloc (pix, n, TY_REAL)
	    do i = 1, n
		Memr[pix+i-1] = i
	}

	do i = 1, nspec {
	    sp = sps[i]
	    call sp_ptype (SP_PTYPE(sp), SP_COLOR(sp), NO, gp, gt)
	    if (wscale)
		call gt_plot (gp, gt, SP_X(sp), SP_Y(sp), SP_NPTS(sp))
	    else
		call gt_plot (gp, gt, Memr[pix], SP_Y(sp), SP_NPTS(sp))
	    x = SP_XLPOS(sp) * xmax + xmin
	    y = SP_YLPOS(sp) * ymax + SP_MEAN(sp)
	    call gtext (gp, x, y, SP_LABEL(sp), "")
	}

	if (!wscale)
	    call mfree (pix, TY_REAL)
end


# SP_PTYPE -- Decode the plotting type and set the GTOOLS structure.

procedure sp_ptype (ptype, color, erase, gp, gt)

char	ptype[ARB]		# Plotting type string
int	color			# Color
int	erase			# Erase plot?
pointer	gp			# GIO pointer
pointer	gt			# GTOOLS pointer

int	i, j, ctoi()
pointer	sp, gttype

begin
	call smark (sp)
	call salloc (gttype, SZ_LINE, TY_CHAR)
	call gt_gets (gt, GTTYPE, Memc[gttype], SZ_LINE)

	i = 1
	if (ctoi (ptype, i, j) > 0) {
	    if (j < 0)
		call gt_sets (gt, GTTYPE, "histogram")
	    else
		call gt_sets (gt, GTTYPE, "line")
	    if (erase == YES)
	        call gt_seti (gt, GTLINE, 0)
	    else
	        call gt_seti (gt, GTLINE, abs(j))
	} else {
	    call gt_sets (gt, GTTYPE, "mark")
	    call gt_sets (gt, GTMARK, ptype)
	    if (erase == YES)
		call gseti (gp, G_PMLTYPE, 0)
	    else
		call gseti (gp, G_PMLTYPE, 1)
	}
	call gt_seti (gt, GTCOLOR, color)

	call sfree (sp)
end


# List of colon commands.
define	CMDS "|show|vshow|step|fraction|move|shift|w0|wpc|velocity|redshift\
		|offset|scale|xlpos|ylpos|label|ulabel|ptype|units|color|"

define	SHOW		1	# Show
define	VSHOW		2	# Verbose show
define	STEP		3	# Separation step
define	FRACTION	4	# Fraction for autolayout
define	MOVE		5	# Move spectrum index
define	SHIFT		6	# Shift spectrum indices
define	WZP		7	# Wavelength zero point
define	WPC		8	# Wavelength per channel
define	VELOCITY	9	# Radial velocity
define	REDSHIFT	10	# Redshift
define	OFFSET		11	# Intensity offset
define	SCALE		12	# Intensity scale
define	XLPOS		13	# X label position
define	YLPOS		14	# Y label position
define	LABEL		15	# Type of labels
define	ULABEL		16	# User label
define	PTYPE		17	# Plot type
define	UNITS		18	# Plot units
define	COLOR		19	# Color

# SP_COLON -- Interpret colon commands.

procedure sp_colon (cmdstr, gp, gt, sps, nspec, units, labels, current, step,
	fraction, redraw)

char	cmdstr[ARB]		# Colon command
pointer	gp			# GIO pointer (used for paging screen)
pointer	gt			# GTOOLS pointer
pointer	sps[ARB]		# Array of spectra structures
int	nspec			# Number of spectra
char	units[SZ_LINE]		# Units string
int	labels			# Label type
int	current			# Current spectrum element (0 if not defined)
real	step			# Separation step
real	fraction		# Fraction for autolayout
int	redraw			# Redraw graph

int	i, j, index, ncmd
real	rval
pointer	stack, cmd, sp, sh, un1, un2

int	nscan(), strdic(), ctoi(), stridxs()
pointer	un_open()

define	done_	10

begin
	call smark (stack)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	# Scan the command string and get the first word.
	call sscan (cmdstr)
	call gargwrd (Memc[cmd], SZ_LINE)

	# Parse the optional spectrum index.  Moving the the end of string.
	# Set the spectrum element to 0 if a non-numeric index is specified.
	# If an index number is given find the appropriate element and print
	# an error if the spectrum index is not defined.
	i = stridxs ("[", Memc[cmd])
	j = 0
	if (i > 0) {
	    Memc[cmd+i-1] = EOS
	    current = 0

	    i = i + 1
	    if (ctoi (Memc[cmd], i, index) > 0) {
		for (i=1; (i<=nspec)&&(SP_INDEX(sps[i])!=index); i=i+1)
		    ;

		current = i
		if (current > nspec) {
		    call printf ("Spectrum %d not defined")
			call pargi (index)
		    call sfree (stack)
		    return
		}
	    }
	    j = current
	}

	# Parse the command.  Print the command if unknown.
	ncmd = strdic (Memc[cmd], Memc[cmd], SZ_LINE, CMDS)

	switch (ncmd) {
	case SHOW: # show spectrum parameters
	    call gargwrd (Memc[cmd], SZ_LINE)
	    if (nscan() == 1)
		call sp_show ("STDOUT", gp, sps, nspec, step)
	    else
		iferr (call sp_show (Memc[cmd], NULL, sps, nspec, step))
		    call erract (EA_WARN)
	case VSHOW: # show spectrum parameters
	    call gargwrd (Memc[cmd], SZ_LINE)
	    if (nscan() == 1)
		call sp_vshow ("STDOUT", gp, sps, nspec, step)
	    else
		iferr (call sp_vshow (Memc[cmd], NULL, sps, nspec, step))
		    call erract (EA_WARN)
	case STEP: # set or show step
	    call gargr (rval)
		if (nscan() == 1) {
		    call printf ("step %g")
		    call pargr (step)
	    } else {
		step = rval
		call sp_scale (sps, nspec, step)
		redraw = YES
	    }
	case FRACTION: # set or show autolayout fraction
	    call gargr (rval)
		if (nscan() == 1) {
		    call printf ("fraction %g")
		    call pargr (fraction)
	    } else
		fraction = rval
	case MOVE: # Move spectrum by index
	    call gargi (index)
	    if (nscan() > 1) {
	        if (current > 0) {
		    sp = sps[current]
		    if (index != SP_INDEX(sp)) {
		        SP_INDEX(sp) = index

		        for (i=current; i<nspec; i=i+1) {
			    sps[i] = sps[i+1]
			    SP_INDEX(sps[i]) = SP_INDEX(sps[i]) - 1
			}
		        for (i=1; (i<nspec)&&(index>SP_INDEX(sps[i])); i=i+1)
			    ;
			for (j=nspec; j>i; j=j-1)
			    sps[j] = sps[j-1]
		        sps[i] = sp
			current = i

			for (j=i; j<nspec; j=j+1) {
			    sp = sps[j+1]
			    if (SP_INDEX(sps[j]) == SP_INDEX(sp))
				SP_INDEX(sp) = SP_INDEX(sp) + 1
			}
    
		        if (labels == LABEL_INDEX)
			    for (i=1; i<=nspec; i=i+1) {
			        sp = sps[i]
		                call sprintf (SP_LABEL(sp), SP_SZLABEL, "%-4d")
				    call pargi (SP_INDEX(sp))
			    }
		        call sp_scale (sps, nspec, step)
			redraw = YES
		    }
		} else
		    call printf ("\007")
	    }
	case SHIFT: # Shift spectra by index
	    call gargi (j)
	    if (nscan() > 1) {
	        if (current > 0) {
		    if (j > 0) {
		        for (i=current; i<=nspec; i=i+1) {
			    sp = sps[i]
			    SP_INDEX(sp) = SP_INDEX(sp) + j
			    call sp_scale (sp, 1, step)
			    call sp_labels (sp, 1, labels)
			    redraw = YES
			}
		    } else if (j < 0) {
		        for (i=current; i>0; i=i-1) {
			    sp = sps[i]
			    SP_INDEX(sp) = SP_INDEX(sp) + j
			    call sp_scale (sp, 1, step)
			    call sp_labels (sp, 1, labels)
			    redraw = YES
			}
		    }
		} else {
		    for (i=1; i<=nspec; i=i+1) {
			sp = sps[i]
			SP_INDEX(sp) = SP_INDEX(sp) + j
			call sp_scale (sp, 1, step)
			call sp_labels (sp, 1, labels)
			redraw = YES
		    }
		}
	    }
	case WZP: # set or show zero point wavelength
	    call gargr (rval)
	    if (current > 0) {
		sp = sps[current]
	        if (nscan() == 1) {
		    call printf ("w0[%d] %g")
			call pargi (SP_INDEX(sp))
			call pargr (SP_W0(sp)*SP_XSCALE(sp)+SP_XOFFSET(sp))
		} else {
		    SP_XOFFSET(sp) = rval - SP_W0(sp) * SP_XSCALE(sp)
		    call sp_scale (sp, 1, step)
		    redraw = YES
		}
	    } else {
		if (nscan() == 1) {
		    call printf ("w0:")
		    do i = 1, nspec {
			sp = sps[i]
			call printf (" %d=%g")
			    call pargi (SP_INDEX(sp))
			    call pargr (SP_W0(sp)*SP_XSCALE(sp)+SP_XOFFSET(sp))
		    }
		} else {
		    do i = 1, nspec {
			sp = sps[i]
			SP_XOFFSET(sp) = rval - SP_W0(sp) * SP_XSCALE(sp)
		        call sp_scale (sp, 1, step)
		        redraw = YES
		    }
		}
	    }
	case WPC: # set or show wavelength per channel
	    call gargr (rval)
	    if (current > 0) {
		sp = sps[current]
	        if (nscan() == 1) {
		    call printf ("wpc[%d] %g")
			call pargi (SP_INDEX(sp))
			call pargr (SP_WPC(sp)*SP_XSCALE(sp))
		} else {
		    SP_WPC(sp) = rval
		    call sp_linear (sp)
		    call sp_scale (sp, 1, step)
		    redraw = YES
		}
	    } else {
		if (nscan() == 1) {
		    call printf ("wpc:")
		    do i = 1, nspec {
			sp = sps[i]
			call printf (" %d=%g")
			    call pargi (SP_INDEX(sp))
			    call pargr (SP_WPC(sp)*SP_XSCALE(sp))
		    }
		} else {
		    do i = 1, nspec {
			sp = sps[i]
			SP_WPC(sp) = rval
			call sp_linear (sp)
		        call sp_scale (sp, 1, step)
		        redraw = YES
		    }
		}
	    }
	case VELOCITY: # set or show radial velocity
	    if (nspec < 0)
		goto done_
	    call gargr (rval)
	    un1 = UN(SP_SH(sps[1]))
	    if (UN_CLASS(un1) == UN_VEL) {
		if (current > 0) {
		    sp = sps[current]
		    if (nscan() == 1) {
			call printf ("velocity[%d] %g")
			    call pargi (SP_INDEX(sp))
			    call pargr (SP_XOFFSET(sp))
		    } else {
			SP_XOFFSET(sp) = rval
			call sp_scale (sp, 1, step)
			redraw = YES
		    }
		} else {
		    if (nscan() == 1) {
			call printf ("velocity:")
			do i = 1, nspec {
			    sp = sps[i]
			    call printf (" %d=%g")
				call pargi (SP_INDEX(sp))
				call pargr (SP_XOFFSET(sp))
			}
		    } else {
			do i = 1, nspec {
			    sp = sps[i]
			    SP_XOFFSET(sp) = rval
			    call sp_scale (sp, 1, step)
			    redraw = YES
			}
		    }
		}
	    } else if (UN_CLASS(un1) != UN_UNKNOWN) {
		if (current > 0) {
		    sp = sps[current]
		    call sprintf (Memc[cmd], SZ_LINE, "km/s %g %s")
			call pargr (SP_W0(sp))
			call pargstr (UN_UNITS(un1))
		    if (nscan() == 1) {
			if (SP_XSCALE(sp) != 1.) {
			    rval = SP_W0(sp) * SP_XSCALE(sp)
			    call un_changer (un1, Memc[cmd], rval, 1, NO)
			} else
			    rval = 0.
			call printf ("velocity[%d] %g")
			    call pargi (SP_INDEX(sp))
			    call pargr (rval)
		    } else {
			un2 = un_open (Memc[cmd])
			call un_ctranr (un2, un1, rval, rval, 1)
			call un_close (un2)
			SP_XSCALE(sp) = rval / SP_W0(sp)
			call sp_scale (sp, 1, step)
			redraw = YES
		    }
		} else {
		    if (nscan() == 1) {
			call printf ("velocity:")
			do i = 1, nspec {
			    sp = sps[i]
			    if (SP_XSCALE(sp) != 1.) {
				call sprintf (Memc[cmd], SZ_LINE, "km/s %g %s")
				    call pargr (SP_W0(sp))
				    call pargstr (UN_UNITS(un1))
				rval = SP_W0(sp) * SP_XSCALE(sp)
				call un_changer (un1, Memc[cmd], rval, 1, NO)
			    } else
				rval = 0.
			    call printf (" %d=%g")
				call pargi (SP_INDEX(sp))
				call pargr (rval)
			}
		    } else {
			do i = 1, nspec {
			    sp = sps[i]
			    call sprintf (Memc[cmd], SZ_LINE, "km/s %g %s")
				call pargr (SP_W0(sp))
				call pargstr (UN_UNITS(un1))
			    un2 = un_open (Memc[cmd])
			    call un_ctranr (un2, un1, rval, rval, 1)
			    call un_close (un1)
			    SP_XSCALE(sp) = rval / SP_W0(sp)
			    call sp_scale (sps[i], 1, step)
			    redraw = YES
			}
		    }
		}
	    }
	case REDSHIFT: # set or show redshift
	    if (nspec < 0)
		goto done_
	    call gargr (rval)
	    un1 = UN(SP_SH(sps[1]))
	    if (UN_CLASS(un1) == UN_VEL) {
		if (current > 0) {
		    sp = sps[current]
		    if (nscan() == 1) {
			call printf ("redshift[%d] %g")
			    call pargi (SP_INDEX(sp))
			    call pargr (SP_XOFFSET(sp)/UN_SCALE(un1))
		    } else {
			SP_XOFFSET(sp) = rval * UN_SCALE(un1)
			call sp_scale (sp, 1, step)
			redraw = YES
		    }
		} else {
		    if (nscan() == 1) {
			call printf ("redshift:")
			do i = 1, nspec {
			    sp = sps[i]
			    call printf (" %d=%g")
				call pargi (SP_INDEX(sp))
				call pargr (SP_XOFFSET(sp)/UN_SCALE(un1))
			}
		    } else {
			do i = 1, nspec {
			    SP_XOFFSET(sp) = rval * UN_SCALE(un1)
			    call sp_scale (sps[i], 1, step)
			    redraw = YES
			}
		    }
		}
	    } else if (UN_CLASS(un1) == UN_WAVE) {
		if (current > 0) {
		    sp = sps[current]
		    if (nscan() == 1) {
			call printf ("redshift[%d] %g")
			    call pargi (SP_INDEX(sp))
			    call pargr (SP_XSCALE(sp)-1)
		    } else {
			rval = 1. + rval
			SP_XSCALE(sp) = rval
			call sp_scale (sp, 1, step)
			redraw = YES
		    }
		} else {
		    if (nscan() == 1) {
			call printf ("redshift:")
			do i = 1, nspec {
			    sp = sps[i]
			    call printf (" %d=%g")
				call pargi (SP_INDEX(sp))
				call pargr (SP_XSCALE(sp)-1)
			}
		    } else {
			rval = 1. + rval
			do i = 1, nspec {
			    SP_XSCALE(sps[i]) = rval
			    call sp_scale (sps[i], 1, step)
			    redraw = YES
			}
		    }
		}
	    } else if (UN_CLASS(un1) == UN_FREQ || UN_CLASS(un1) == UN_ENERGY) {
		if (current > 0) {
		    sp = sps[current]
		    if (nscan() == 1) {
			call printf ("redshift[%d] %g")
			    call pargi (SP_INDEX(sp))
			    call pargr (1./SP_XSCALE(sp)-1)
		    } else {
			rval = 1. / (1. + rval)
			SP_XSCALE(sp) = rval
			call sp_scale (sp, 1, step)
			redraw = YES
		    }
		} else {
		    if (nscan() == 1) {
			call printf ("redshift:")
			do i = 1, nspec {
			    sp = sps[i]
			    call printf (" %d=%g")
				call pargi (SP_INDEX(sp))
				call pargr (1./SP_XSCALE(sp)-1)
			}
		    } else {
			rval = 1./ (1. + rval)
			do i = 1, nspec {
			    SP_XSCALE(sps[i]) = rval
			    call sp_scale (sps[i], 1, step)
			    redraw = YES
			}
		    }
		}
	    }
	case OFFSET: # set or show intensity offset
	    call gargr (rval)
	    if (current > 0) {
		sp = sps[current]
	        if (nscan() == 1) {
		    call printf ("offset[%d] %g")
			call pargi (SP_INDEX(sp))
			call pargr (SP_OFFSET(sp))
		} else {
		    SP_OFFSET(sp) = rval
		    call sp_scale (sp, 1, step)
		    redraw = YES
		}
	    } else {
		if (nscan() == 1) {
		    call printf ("offset:")
		    do i = 1, nspec {
			sp = sps[i]
			call printf (" %d=%g")
			    call pargi (SP_INDEX(sp))
			    call pargr (SP_OFFSET(sp))
		    }
		} else {
		    do i = 1, nspec {
			SP_OFFSET(sps[i]) = rval
		        call sp_scale (sps[i], 1, step)
		        redraw = YES
		    }
		}
	    }
	case SCALE: # set or show intensity scale
	    call gargr (rval)
	    if (current > 0) {
		sp = sps[current]
	        if (nscan() == 1) {
		    call printf ("scale[%d] %g")
			call pargi (SP_INDEX(sp))
			call pargr (SP_SCALE(sp))
		} else {
		    SP_SCALE(sp) = rval
		    call sp_scale (sp, 1, step)
		    redraw = YES
		}
	    } else {
		if (nscan() == 1) {
		    call printf ("scale:")
		    do i = 1, nspec {
			sp = sps[i]
			call printf (" %d=%g")
			    call pargi (SP_INDEX(sp))
			    call pargr (SP_SCALE(sp))
		    }
		} else {
		    do i = 1, nspec {
			SP_SCALE(sps[i]) = rval
		        call sp_scale (sps[i], 1, step)
		        redraw = YES
		    }
		}
	    }
	case XLPOS: # set or show X label position
	    call gargr (rval)
	    if (current > 0) {
		sp = sps[current]
	        if (nscan() == 1) {
		    call printf ("xlpos[%d] %g")
			call pargi (SP_INDEX(sp))
			call pargr (SP_XLPOS(sp))
		} else {
		    SP_XLPOS(sp) = rval
		    redraw = YES
		}
	    } else {
		if (nscan() == 1) {
		    call printf ("xlpos:")
		    do i = 1, nspec {
			sp = sps[i]
			call printf (" %d=%g")
			    call pargi (SP_INDEX(sp))
			    call pargr (SP_XLPOS(sp))
		    }
		} else {
		    do i = 1, nspec {
			SP_XLPOS(sps[i]) = rval
		        redraw = YES
		    }
		}
	    }
	case YLPOS: # set or show Y label position
	    call gargr (rval)
	    if (current > 0) {
		sp = sps[current]
	        if (nscan() == 1) {
		    call printf ("ylpos[%d] %g")
			call pargi (SP_INDEX(sp))
			call pargr (SP_YLPOS(sp))
		} else {
		    SP_YLPOS(sp) = rval
		    redraw = YES
		}
	    } else {
		if (nscan() == 1) {
		    call printf ("ylpos:")
		    do i = 1, nspec {
			sp = sps[i]
			call printf (" %d=%g")
			    call pargi (SP_INDEX(sp))
			    call pargr (SP_YLPOS(sp))
		    }
		} else {
		    do i = 1, nspec {
			SP_YLPOS(sps[i]) = rval
		        redraw = YES
		    }
		}
	    }
	case LABEL: # Set or show label type
	    call gargwrd (Memc[cmd], SZ_LINE)
	    if (nscan() == 1) {
		switch (labels) {
		case LABEL_NONE:
		    call printf ("labels none")
		case LABEL_IMNAME:
		    call printf ("labels imname")
		case LABEL_IMTITLE:
		    call printf ("labels imtitle")
		case LABEL_INDEX:
		    call printf ("labels index")
		case LABEL_USER:
		    call printf ("labels user")
		}
	    } else {
		ncmd = strdic (Memc[cmd], Memc[cmd], SZ_LINE, LABELS)
		if (ncmd == 0) {
		    call printf ("Unknown label type: %s")
			call pargstr (Memc[cmd])
		} else {
		    labels = ncmd
		    call sp_labels (sps, nspec, labels)
		}
	    }
	case ULABEL: # Set or show user labels
	    call gargwrd (Memc[cmd], SZ_LINE)
	    if (current > 0) {
		sp = sps[current]
	        if (nscan() == 1) {
		    call printf ("ulabel[%d] %s")
			call pargi (SP_INDEX(sp))
			call pargstr (SP_ULABEL(sp))
		} else {
		    call reset_scan ()
		    call gargwrd (Memc[cmd], SZ_LINE)
		    call gargstr (Memc[cmd], SZ_LINE)
		    call strcpy (Memc[cmd], SP_ULABEL(sp), SP_SZULABEL)
		    if (labels == LABEL_USER)
		        call strcpy (SP_ULABEL(sp), SP_LABEL(sp), SP_SZLABEL)
		}
	    } else {
		if (nscan() == 1) {
		    call printf ("ulabel:")
		    do i = 1, nspec {
			sp = sps[i]
			call printf (" %d=%s")
			    call pargi (SP_INDEX(sp))
			    call pargstr (SP_ULABEL(sp))
		    }
		} else {
		    call reset_scan ()
		    call gargwrd (Memc[cmd], SZ_LINE)
		    call gargstr (Memc[cmd], SZ_LINE)
		    do i = 1, nspec {
			sp = sps[i]
			call strcpy (Memc[cmd], SP_ULABEL(sp), SP_SZULABEL)
		        if (labels == LABEL_USER)
		            call strcpy (SP_ULABEL(sp), SP_LABEL(sp),SP_SZLABEL)
		    }
		}
	    }
	case PTYPE: # Set or show plotting type
	    call gargwrd (Memc[cmd], SZ_LINE)
	    if (current > 0) {
		sp = sps[current]
	        if (nscan() == 1) {
		    call printf ("ptype[%d] %s")
			call pargi (SP_INDEX(sp))
			call pargstr (SP_PTYPE(sp))
		} else {
		    call strcpy (Memc[cmd], SP_PTYPE(sp), SP_SZPTYPE)
		}
	    } else {
		if (nscan() == 1) {
		    call printf ("ptype:")
		    do i = 1, nspec {
			sp = sps[i]
			call printf (" %d=%s")
			    call pargi (SP_INDEX(sp))
			    call pargstr (SP_PTYPE(sp))
		    }
		} else {
		    do i = 1, nspec
			call strcpy (Memc[cmd], SP_PTYPE(sps[i]),
			    SP_SZPTYPE)
		}
	    }
	case UNITS: # Change plotting units
	    # Any change of units resets the offset and scale parametes.
	    call gargstr (Memc[cmd], SZ_LINE)
	    iferr {
		do i = 1, nspec {
		    if (j > 0 && i != j)
			next
		    sp = sps[i]
		    sh = SP_SH(sp) 
		    call un_changer (UN(sh), Memc[cmd], Memr[SX(sh)],
			SN(sh), YES)
		    SP_W0(sp) = Memr[SX(sh)]
		    SP_WPC(sp) = (Memr[SX(sh)+SN(sh)-1] - Memr[SX(sh)]) /
			(SN(sh) - 1)
		    SP_XSCALE(sp) = 1.
		    SP_XOFFSET(sp) = 0.
		    call sp_scale (sp, 1, step)
		    if (i == 1) {
			call strcpy (Memc[cmd], units, SZ_FNAME)
			call gt_sets (gt, GTXLABEL, UN_LABEL(UN(sh)))
			call gt_sets (gt, GTXUNITS, UN_UNITS(UN(sh)))
		    }
		    redraw = YES
		}
	    } then
		call erract (EA_WARN)
	case COLOR: # Set or show color
	    call gargi (j)
	    if (current > 0) {
		sp = sps[current]
	        if (nscan() == 1) {
		    call printf ("color[%d] %d")
			call pargi (SP_INDEX(sp))
			call pargi (SP_COLOR(sp))
		} else {
		    SP_COLOR(sp) = j
		}
	    } else {
		if (nscan() == 1) {
		    call printf ("color:")
		    do i = 1, nspec {
			sp = sps[i]
			call printf (" %d=%d")
			    call pargi (SP_INDEX(sp))
			    call pargi (SP_COLOR(sp))
		    }
		} else {
		    do i = 1, nspec
			SP_COLOR(sps[i]) = j
		}
	    }
	default: # Print unknown command
	    call printf ("Unknown command: %s\007")
		call pargstr (cmdstr)
	}

done_	call sfree (stack)
end


# SP_GDATA -- Get spectrum and add it to the array of spectrum structures.
# Return an error if the image is not found.  If a two or three dimensional
# image enter each line.  The spectrum data kept in memory and the image is
# closed.

procedure sp_gdata (image, units, current, sps, nspec)

char	image[ARB]		# Image name
char	units[ARB]		# Coordinate units
int	current			# Element to append
pointer	sps			# Pointer to array of spectra structures
int	nspec			# Number of spectra

real	scale			# Default intensity scale
real	offset			# Default intensity offset
real	xlpos, ylpos		# Default position of labels
char	ptype[SP_SZPTYPE]	# Default plot type

int	i, j, k, l, m, trans
pointer	sp, im, mw, sh, stack, aps, bands, str, ptr

int	ctor(), open(), fscan(), nowhite(), clgwrd()
bool	rng_elementi(), fp_equalr()
real	clgetr(), asumr(), imgetr(), sp_logerr()
pointer	immap(), smw_openim(), rng_open()

errchk	immap, smw_openim, open

extern	sp_logerr

begin
	call smark (stack)
	call salloc (str, SZ_LINE, TY_CHAR)
 
	# Map the image and return an error if this fails.
	im = immap (image, READ_ONLY, 0)
	mw = smw_openim (im)

	# Get parameters.
	if (nspec == 0) {
	    #scale = clgetr ("scale")
	    #offset = clgetr ("offset")
	    xlpos = clgetr ("xlpos")
	    ylpos = clgetr ("ylpos")
	    call clgstr ("ptype", ptype, SP_SZPTYPE)
	    trans = clgwrd ("transform", Memc[str], SZ_LINE, TRANSFORMS)    
	}

	call clgstr ("scale", Memc[str], SZ_LINE)
	if (nowhite (Memc[str], Memc[str], SZ_LINE) == 0)
	    call error (1, "Error in scale parameter")
	if (Memc[str] == '@') {
	    j = open (Memc[str+1], READ_ONLY, TEXT_FILE)
	    do i = 1, nspec+1
		if (fscan(j) == EOF)
		    call error (1, "Error reading scale file")
	    call gargr (scale)
	    call close (j)
	} else if (IS_ALPHA(Memc[str])) {
	    scale = imgetr (im, Memc[str])
	} else {
	    i = 1
	    if (ctor (Memc[str], i, scale) == 0)
		call error (1, "Error in scale parameter")
	}

	call clgstr ("offset", Memc[str], SZ_LINE)
	if (nowhite (Memc[str], Memc[str], SZ_LINE) == 0)
	    call error (1, "Error in offset parameter")
	if (Memc[str] == '@') {
	    j = open (Memc[str+1], READ_ONLY, TEXT_FILE)
	    do i = 1, nspec+1
		if (fscan(j) == EOF)
		    call error (1, "Error reading offset file")
	    call gargr (offset)
	    call close (j)
	} else if (IS_ALPHA(Memc[str]))
	    offset = imgetr (im, Memc[str])
	else {
	    i = 1
	    if (ctor (Memc[str], i, offset) == 0)
		call error (1, "Error in offset parameter")
	}

	call clgstr ("apertures", Memc[str], SZ_LINE)
	iferr (aps = rng_open (Memc[str], INDEF, INDEF, INDEF))
	    call error (0, "Bad aperture/record list")
	call clgstr ("bands", Memc[str], SZ_LINE)
	iferr (bands = rng_open (Memc[str], INDEF, INDEF, INDEF))
	    call error (0, "Bad band list")

	# For each line in the image, allocate memory for the spectrum
	# structure, get the pixel data, compute the mean and limits,
	# set the structure parameters, and add the structure to the
	# array of structures.

	do j = 1, SMW_NBANDS(mw) {
	    if (SMW_FORMAT(mw) != SMW_ND)
		if (!rng_elementi (bands, j))
		    next
	    do i = 1, SMW_NSPEC(mw) {
		if (SMW_FORMAT(mw) == SMW_ND) {
		    call smw_mw (mw, i, j, ptr, k, l)
		    if (!rng_elementi (aps, k) || !rng_elementi (bands, l))
			next
		} else {
		    call shdr_open (im, mw, i, j, INDEFI, SHHDR, sh)
		    if (!rng_elementi (aps, AP(sh)))
			next
		}
		call shdr_open (im, mw, i, j, INDEFI, SHDATA, sh)
		iferr (call shdr_units (sh, units))
		    ;

	        call sp_alloc (sp, sh)
	        SP_NPTS(sp) = SN(sh)
		SP_W0(sp) = Memr[SX(sh)]
		SP_WPC(sp) = (Memr[SX(sh)+SN(sh)-1] - Memr[SX(sh)]) /
		    (SN(sh) - 1)
		switch (trans) {
		case TRANS_LOG:
		    SP_OMIN(sp) = MAX_REAL; SP_OMAX(sp) = -MAX_REAL
		    ptr = SY(sh);
		    do m = 1, SP_NPTS(sp) {
		        if (Memr[ptr] > 0.) {
			    SP_OMIN(sp) = min (SP_OMIN(sp), Memr[ptr])
			    SP_OMAX(sp) = max (SP_OMAX(sp), Memr[ptr])
			}
			ptr = ptr + 1
		    }
		    if (SP_OMAX(sp) > 0.) {
		        call amaxkr (Memr[SY(sh)], SP_OMIN(sp), Memr[SY(sh)],
			    SN(sh))
			call alogr (Memr[SY(sh)], Memr[SY(sh)], SN(sh),
			    sp_logerr)
			call amovr (Memr[SY(sh)], Memr[SY(SP_SH(sp))], SN(sh))
		    }
		}
	        SP_OMEAN(sp) = asumr (Memr[SY(sh)], SN(sh)) / SN(sh)
	        call alimr (Memr[SY(sh)], SN(sh), SP_OMIN(sp), SP_OMAX(sp))

		SP_XSCALE(sp) = 1.
		SP_XOFFSET(sp) = 0.
	        SP_SCALE(sp) = scale
	        SP_OFFSET(sp) = offset
	        SP_XLPOS(sp) = xlpos
	        SP_YLPOS(sp) = ylpos
		SP_COLOR(sp) = 1

		call sprintf (SP_IMNAME(sp), SP_SZNAME, "%s%s(%d)")
		    call pargstr (IMNAME(sh))
		    call pargstr (IMSEC(sh))
		    call pargi (AP(sh))
		call strcpy (TITLE(sh), SP_IMTITLE(sp), SP_SZTITLE)
	        call strcpy (ptype, SP_PTYPE(sp), SP_SZPTYPE)
	        SP_ULABEL(sp) = EOS

	        call sp_add (sp, current, sps, nspec)
	    }
	}

	# Close the image.
	call shdr_close (sh)
	call rng_close (bands)
	call rng_close (aps)
	call smw_close (mw)
	call imunmap (im)
 
	call sfree (stack)
end


# SP_LINEAR -- Reset linear coordinates

procedure sp_linear (sp)

pointer	sp			# SPECPLOT pointer

int	i
pointer	x

begin
	x = SX(SP_SH(sp))
	do i = 0, SP_NPTS(sp)-1
	    Memr[x+i] = SP_W0(sp) + i * SP_WPC(sp)
	SP_XSCALE(sp) = 1.
	SP_XOFFSET(sp) = 0.
end


# SP_DELETE -- Delete a spectrum from memory.  The index numbers are
# decreased to fill the hole.

procedure sp_delete (current, sps, nspec)

int	current			# Element to be deleted
pointer	sps			# Pointer to array of spectrum structures
int	nspec			# Number of spectra

int	i

begin
	if (nspec == 0)
	    return

	for (i = current; i < nspec; i = i + 1) {
	    Memi[sps+i-1] = Memi[sps+i]
	    SP_INDEX(Memi[sps+i-1]) = SP_INDEX(Memi[sps+i-1]) - 1
	}
	nspec = nspec - 1
end


# SP_ADD --  Add a spectrum structure to the array of structures
# following the specified element.  The spectrum index is defined to be
# one higher than the spectrum to be followed and all higher indexed
# spectra are increased by 1.  Special cases are when there are no
# spectra in which case the index is set to 1 and when the current
# element to be followed is zero.  The current element is set to the
# added spectrum.  The array of pointers is expanded in blocks of 100.

procedure sp_add (sp, current, sps, nspec)

pointer	sp			# Spectrum structure to be appended
int	current			# Element followed (in), added element (out)
pointer	sps			# Pointer to array of spectrum structures
int	nspec			# Number of spectra

int	i

begin
	# Reallocate memory for the array of structure pointers in steps of 100.
	if (mod (nspec, 100) == 0)
	    call realloc (sps, nspec + 100, TY_POINTER)

	# Shift higher spectra in the array and increase the index numbers by 1
	# and then add the new spectrum pointer. 
	for (i = nspec; i > current; i = i - 1) {
	    Memi[sps+i] = Memi[sps+i-1]
	    SP_INDEX(Memi[sps+i]) = SP_INDEX(Memi[sps+i]) + 1
	}
	Memi[sps+current] = sp

	# Set the new spectrum index.
	if (nspec == 0)
	    SP_INDEX(sp) = 1
	else if (current == 0)
	    SP_INDEX(sp) = SP_INDEX(Memi[sps+current+1]) - 1
	else
	    SP_INDEX(sp) = SP_INDEX(Memi[sps+current-1]) + 1

	# Adjust the current element and number of spectra.
	current = current + 1
	nspec = nspec + 1
end


# SP_LABELS -- Set the spectrum labels to the specified type.

procedure sp_labels (sps, nspec, labels)

pointer	sps[ARB]	# Spectrum pointers
int	nspec		# Number of spectra
int	labels		# Type of labels

int	i

begin
	for (i = 1; i <= nspec; i = i + 1) {
	    switch (labels) {
	    case LABEL_NONE:
		SP_LABEL(sps[i]) = EOS
	    case LABEL_IMNAME:
		call strcpy (SP_IMNAME(sps[i]), SP_LABEL(sps[i]), SP_SZLABEL)
	    case LABEL_IMTITLE:
		call strcpy (SP_IMTITLE(sps[i]), SP_LABEL(sps[i]), SP_SZLABEL)
	    case LABEL_INDEX:
		call sprintf (SP_LABEL(sps[i]), SP_SZLABEL, "%-4d")
		    call pargi (SP_INDEX(sps[i]))
	    case LABEL_USER:
		call strcpy (SP_ULABEL(sps[i]), SP_LABEL(sps[i]), SP_SZULABEL)
	    }
	}
end


# SP_ALLOC -- Allocate memory for a spectrum structure with given number of
# data points.  The MWCS is not used.

procedure sp_alloc (sp, sh)

pointer	sp		# Spectrum structure pointer to be allocated
pointer	sh		# Spectrum header pointer

begin
	call calloc (sp, SP_LEN, TY_STRUCT)
	call calloc (SP_PX(sp), SN(sh), TY_REAL)
	call calloc (SP_PY(sp), SN(sh), TY_REAL)

	call shdr_copy (sh, SP_SH(sp), NO)
	MW(SP_SH(sp)) = NULL
end


# SP_FREE -- Free a spectrum structure.

procedure sp_free (sp)

pointer	sp, sh		# Spectrum structure pointers

begin
	sh = SP_SH(sp)
	call shdr_close (sh)

	call mfree (SP_PX(sp), TY_REAL)
	call mfree (SP_PY(sp), TY_REAL)
	call mfree (sp, TY_STRUCT)
end


# SP_NEAREST -- Find the nearest spectrum to the cursor and return the element.
# Return zero if no spectra are defined.  The distance is in NDC.

int procedure sp_nearest (gp, wx1, wy1, key, cmd, sps, nspec)

pointer	gp			# GIO pointer
real	wx1, wy1		# Cursor position
int	key			# Key
char	cmd[ARB]		# Cursor command
pointer	sps[ARB]		# Array of structure pointers
int	nspec			# Number of spectra

int	i, j, k, stridxs()
real	wx0, wy0, x0, y0, x1, y1, r2, r2min
pointer	sp, px, py

begin
	# Check for explicit specification.
	if (key == ':') {
	    if (stridxs ("[", cmd) > 0)
		return (1)
	}

	if (IS_INDEFR(wx1))
	    wx1 = 0.
	if (IS_INDEFR(wy1))
	    wy1 = 0.

	# Transform world cursor coordinates to NDC.
	call gctran (gp, wx1, wy1, wx0, wy0, 1, 0)

	# Search for nearest point.
	k = 0
	r2min = MAX_REAL
	do i = 1, nspec {
	    sp = sps[i]
	    px = SP_PX(sp) - 1
	    py = SP_PY(sp) - 1
	    do j = 1, SP_NPTS(sp) {
		x1 = Memr[px + j]
		y1 = Memr[py + j]
	        call gctran (gp, x1, y1, x0, y0, 1, 0)
	        r2 = (x0 - wx0) ** 2 + (y0 - wy0) ** 2
	        if (r2 < r2min) {
		    r2min = r2
		    k = i
		}
	    }
	}

	return (k)
end


# SP_SHOW -- Show parameter information.  Clear the screen if the output is
# to the graphics device otherwise append to the specified file.

procedure sp_show (file, gp, sps, nspec, step)

char	file[ARB]		# Optional file
pointer	gp			# Graphics pointer
pointer	sps[ARB]		# Spectra data
int	nspec			# Number of spectra
real	step			# Separation step

int	i, fd
pointer	stack, line, sp

int	open()
errchk	open()

begin
	fd = open (file, APPEND, TEXT_FILE)
	if (gp != NULL)
	    call gdeactivate (gp, AW_CLEAR)

	call smark (stack)
	call salloc (line, SZ_LINE, TY_CHAR)
	call sysid (Memc[line], SZ_LINE)
	call fprintf (fd, "%s\n\n")
	call pargstr (Memc[line])

	call fprintf (fd, "Step = %g\n\n")
	    call pargr (step)
	call fprintf (fd, "   %16s %7s %7s %7s %7s   %s\n")
	    call pargstr ("Image Name")
	    call pargstr ("W0")
	    call pargstr ("WPC")
	    call pargstr ("Offset")
	    call pargstr ("Scale")
	    call pargstr ("Title")

	do i = 1, nspec {
	    sp = sps[i]
	    call fprintf (fd, "%2d %16s %7g %7g %7g %7g   %s\n")
		call pargi (SP_INDEX(sp))
		call pargstr (SP_IMNAME(sp))
		call pargr (SP_W0(sp)*SP_XSCALE(sp)+SP_XOFFSET(sp))
		call pargr (SP_WPC(sp)*SP_XSCALE(sp))
		call pargr (SP_OFFSET(sp))
		call pargr (SP_SCALE(sp))
		call pargstr (SP_IMTITLE(sp))
	}

	call sfree (stack)

	call close (fd)
	if (gp != NULL)
	    call greactivate (gp, AW_PAUSE)
end


# SP_VSHOW -- Show verbose parameter information.  Clear the screen if the
# output is to the graphics device otherwise append to the specified file.

procedure sp_vshow (file, gp, sps, nspec, step)

char	file[ARB]		# Optional file
pointer	gp			# Graphics pointer
pointer	sps[ARB]		# Spectra data
int	nspec			# Numbeer of spectra
real	step			# Separation step

int	i, fd
real	z, v
pointer	stack, line, sp, un

int	open()
errchk	open()

begin
	fd = open (file, APPEND, TEXT_FILE)
	if (gp != NULL)
	    call gdeactivate (gp, AW_CLEAR)

	call smark (stack)
	call salloc (line, SZ_LINE, TY_CHAR)
	call sysid (Memc[line], SZ_LINE)
	call fprintf (fd, "%s\n\n")
	call pargstr (Memc[line])

	call fprintf (fd, "Step = %g\n")
	    call pargr (step)
	call fprintf (fd, "\n   %16s %7s %7s %7s %7s   %s\n")
	    call pargstr ("Image Name")
	    call pargstr ("W0")
	    call pargstr ("WPC")
	    call pargstr ("Offset")
	    call pargstr ("Scale")
	    call pargstr ("Title")

	do i = 1, nspec {
	    sp = sps[i]
	    call fprintf (fd, "%2d %16s %7g %7g %7g %7g   %s\n")
		call pargi (SP_INDEX(sp))
		call pargstr (SP_IMNAME(sp))
		call pargr (SP_W0(sp)*SP_XSCALE(sp)+SP_XOFFSET(sp))
		call pargr (SP_WPC(sp)*SP_XSCALE(sp))
		call pargr (SP_OFFSET(sp))
		call pargr (SP_SCALE(sp))
		call pargstr (SP_IMTITLE(sp))
	}

	call fprintf (fd, "\n   %16s %9s %9s %9s %9s\n")
	    call pargstr ("Image Name")
	    call pargstr ("Mean")
	    call pargstr ("DW0")
	    call pargstr ("Z")
	    call pargstr ("V(km/s)")

	un = UN(SP_SH(sps[1]))
	if (UN_CLASS(un) == UN_VEL) {
	    do i = 1, nspec {
		sp = sps[i]
		z = SP_XOFFSET(sp) / UN_SCALE(un)
		v = SP_XOFFSET(sp)
		call fprintf (fd, "%2d %16s %9g %9g %9g %9g\n")
		    call pargi (SP_INDEX(sp))
		    call pargstr (SP_IMNAME(sp))
		    call pargr (SP_OMEAN(sp))
		    call pargr (SP_XOFFSET(sp))
		    call pargr (z)
		    call pargr (v)
	    }
	} else if (UN_CLASS(un) == UN_WAVE) {
	    do i = 1, nspec {
		sp = sps[i]
		if (SP_XSCALE(sp) != 1.) {
		    call sprintf (Memc[line], SZ_LINE, "km/s %g %s")
			call pargr (SP_W0(sp))
			call pargstr (UN_UNITS(un))
		    z = SP_XSCALE(sp) - 1
		    v = SP_W0(sp) * SP_XSCALE(sp)
		    call un_changer (un, Memc[line], v, 1, NO)
		} else {
		    z = 0.
		    v = 0.
		}
		call fprintf (fd, "%2d %16s %9g %9g %9g %9g\n")
		    call pargi (SP_INDEX(sp))
		    call pargstr (SP_IMNAME(sp))
		    call pargr (SP_OMEAN(sp))
		    call pargr (SP_XOFFSET(sp))
		    call pargr (z)
		    call pargr (v)
	    }
	} else if (UN_CLASS(un) == UN_FREQ || UN_CLASS(un) == UN_ENERGY) {
	    do i = 1, nspec {
		sp = sps[i]
		if (SP_XSCALE(sp) != 1.) {
		    call sprintf (Memc[line], SZ_LINE, "km/s %g %s")
			call pargr (SP_W0(sp))
			call pargstr (UN_UNITS(un))
		    z = 1. / SP_XSCALE(sp) - 1
		    v = SP_W0(sp) * SP_XSCALE(sp)
		    call un_changer (un, Memc[line], v, 1, NO)
		} else {
		    z = 0.
		    v = 0.
		}
		call fprintf (fd, "%2d %16s %9g %9g %9g %9g\n")
		    call pargi (SP_INDEX(sp))
		    call pargstr (SP_IMNAME(sp))
		    call pargr (SP_OMEAN(sp))
		    call pargr (SP_XOFFSET(sp))
		    call pargr (z)
		    call pargr (v)
	    }
	}

	call sfree (stack)

	call close (fd)
	if (gp != NULL)
	    call greactivate (gp, AW_PAUSE)
end


# SP_LOGERR -- Value for non-positive values in log function.

real procedure sp_logerr (x)

real	x

begin
	return (0.)
end
