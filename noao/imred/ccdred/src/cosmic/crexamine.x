include	<error.h>
include	<syserr.h>
include <imhdr.h>
include	<gset.h>
include	<mach.h>
include	<pkg/gtools.h>
include	"crlist.h"

# CR_EXAMINE  -- Examine cosmic ray candidates interactively.
# CR_GRAPH    -- Make a graph
# CR_NEAREST  -- Find the nearest cosmic ray to the cursor.
# CR_DELETE   -- Set replace flag for cosmic ray candidate nearest cursor.
# CR_UNDELETE -- Set no replace flag for cosmic ray candidate nearest cursor.
# CR_UPDATE   -- Change replacement flags, thresholds, and graphs.
# CR_PLOT     -- Make log plot

define	HELP	"noao$lib/scr/cosmicrays.key"
define	PROMPT	"cosmic ray options"

# CR_EXAMINE -- Examine cosmic ray candidates interactively.

procedure cr_examine (cr, im, fluxratio)

pointer	cr			# Cosmic ray list
pointer	im			# Image pointer
real	fluxratio		# Flux ratio threshold

char	cmd[SZ_LINE]
int	i, newgraph, wcs, key, nc, nl, c1, c2, l1, l2
real	wx, wy
pointer	sp, fname, gp, gt, data

int	clgcur()
pointer	gopen(), gt_init(), imgs2r()
errchk	gopen

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)

	# Set up the graphics.
	call clgstr ("graphics", Memc[fname], SZ_FNAME)
	gp = gopen (Memc[fname], NEW_FILE, STDGRAPH)
	gt = gt_init()
	call gt_sets (gt, GTTYPE, "mark")
	call gt_sets (gt, GTXTRAN, "log")
	call gt_setr (gt, GTXMIN, 10.)
	call gt_setr (gt, GTYMIN, 0.)
	call gt_sets (gt, GTTITLE, "Parameters of cosmic rays candidates")
	call gt_sets (gt, GTPARAMS, IM_TITLE(im))
	call gt_sets (gt, GTXLABEL, "Flux")
	call gt_sets (gt, GTYLABEL, "Flux Ratio")

	# Set image limits
	nc = IM_LEN(im, 1)
	nl = IM_LEN(im, 2)

	# Enter cursor loop.
	key = 'r'
	repeat {
	    switch (key) {
	    case '?': # Print help text.
		call gpagefile (gp, HELP, PROMPT)
	    case ':': # Colon commands.
		switch (cmd[1]) {
		case '/':
		    call gt_colon (cmd, gp, gt, newgraph)
		default:
		    call printf ("\007")
		}
	    case 'd': # Delete candidate
		call cr_delete (gp, wx, wy, cr, i)
	    case 'q': # Quit
		break
	    case 'r': # Redraw the graph.
		newgraph = YES
	    case 's': # Make surface plots
		call cr_nearest (gp, wx, wy, cr, i)
		c1 = max (1, int (Memr[CR_COL(cr)+i-1]) - 5)
		c2 = min (nc, int (Memr[CR_COL(cr)+i-1]) + 5)
		l1 = max (1, int (Memr[CR_LINE(cr)+i-1]) - 5)
		l2 = min (nl, int (Memr[CR_LINE(cr)+i-1]) + 5)
		data = imgs2r (im, c1, c2, l1, l2)
		call gclear (gp)
		call gsview (gp, 0.03, 0.48, 0.53, 0.98)
		call cr_surface (gp, Memr[data], c2-c1+1, l2-l1+1, -33., 25.)
		call gsview (gp, 0.53, 0.98, 0.53, 0.98)
		call cr_surface (gp, Memr[data], c2-c1+1, l2-l1+1, -123., 25.)
		call gsview (gp, 0.03, 0.48, 0.03, 0.48)
		call cr_surface (gp, Memr[data], c2-c1+1, l2-l1+1, 57., 25.)
		call gsview (gp, 0.53, 0.98, 0.03, 0.48)
		call cr_surface (gp, Memr[data], c2-c1+1, l2-l1+1, 147., 25.)
		call fprintf (STDERR, "[Type any key to continue]")
		i = clgcur ("cursor", wx, wy, wcs, key, cmd, SZ_LINE)
		newgraph = YES
	    case 't': # Set threshold
		call cr_update (gp, wy, cr, fluxratio)
		call clputr ("fluxratio", fluxratio)
	    case 'u': # Undelete candidate
		call cr_undelete (gp, wx, wy, cr, i)
	    case 'w':# Window the graph.
		call gt_window (gt, gp, "cursor", newgraph)
	    default: # Ring bell for unrecognized commands.
		call printf ("\007")
	    }

	    # Update the graph if needed.
	    if (newgraph == YES) {
		call cr_graph (gp, gt, cr, fluxratio)
	        newgraph = NO
	    }
	} until (clgcur ("cursor", wx, wy, wcs, key, cmd, SZ_LINE) == EOF)

	call gt_free (gt)
	call gclose (gp)
	call sfree (sp)
end


# CR_GRAPH -- Make a graph

procedure cr_graph (gp, gt, cr, fluxratio)

pointer	gp		# GIO pointer
pointer	gt		# GTOOLS pointers
pointer	cr		# Cosmic ray list
real	fluxratio	# Flux ratio threshold

int	i, ncr
real	x1, x2, y1, y2
pointer	x, y, flag

begin
	ncr = CR_NCR(cr)
	x = CR_FLUX(cr) - 1
	y = CR_RATIO(cr) - 1
	flag = CR_FLAG(cr) - 1

	call gclear (gp)
	call gt_ascale (gp, gt, Memr[x+1], Memr[y+1], ncr)
	call gt_swind (gp, gt)
	call gt_labax (gp, gt)

	do i = 1, ncr
	    if ((Memi[flag+i] == NO) || (Memi[flag+i] == ALWAYSNO))
		call gmark (gp, Memr[x+i], Memr[y+i], GM_PLUS, 2., 2.)
	    else
		call gmark (gp, Memr[x+i], Memr[y+i], GM_CROSS, 2., 2.)

	call ggwind (gp, x1, x2, y1, y2)
	call gseti (gp, G_PLTYPE, 2)
	call gline (gp, x1, fluxratio, x2, fluxratio)
end


# CR_NEAREST -- Find the nearest cosmic ray to the cursor.

procedure cr_nearest (gp, wx, wy, cr, nearest)

pointer	gp		# GIO pointer
real	wx, wy		# Cursor position
pointer	cr		# Cosmic ray list
int	nearest		# Index of nearest point (returned)

int	i, ncr
real	x0, y0, x1, y1, x2, y2, r2, r2min
pointer	x, y

begin
	ncr = CR_NCR(cr)
	x = CR_FLUX(cr) - 1
	y = CR_RATIO(cr) - 1

	# Search for nearest point in NDC.
	r2min = MAX_REAL
	call gctran (gp, wx, wy, wx, wy, 1, 0)
	do i = 1, ncr {
	    x1 = Memr[x+i]
	    y1 = Memr[y+i]
	    call gctran (gp, x1, y1, x0, y0, 1, 0)
	    r2 = (x0 - wx) ** 2 + (y0 - wy) ** 2
	    if (r2 < r2min) {
		r2min = r2
		x2 = x1
		y2 = y1
		nearest = i
	    }
	}

	# Move the cursor to the selected point.
	call gscur (gp, x2, y2)
end


# CR_DELETE -- Set replace flag for cosmic ray candidate nearest cursor.

procedure cr_delete (gp, wx, wy, cr, nearest)

pointer	gp		# GIO pointer
real	wx, wy		# Cursor position
pointer	cr		# Cosmic ray list
int	nearest		# Index of nearest point (returned)

int	i, ncr
real	x0, y0, x1, y1, x2, y2, r2, r2min
pointer	x, y, flag

begin
	ncr = CR_NCR(cr)
	x = CR_FLUX(cr) - 1
	y = CR_RATIO(cr) - 1
	flag = CR_FLAG(cr) - 1

	# Search for nearest point in NDC.
	nearest = 0
	r2min = MAX_REAL
	call gctran (gp, wx, wy, wx, wy, 1, 0)
	do i = 1, ncr {
	    if ((Memi[flag+i] == YES) || (Memi[flag+i] == ALWAYSYES))
		next
	    x1 = Memr[x+i]
	    y1 = Memr[y+i]
	    call gctran (gp, x1, y1, x0, y0, 1, 0)
	    r2 = (x0 - wx) ** 2 + (y0 - wy) ** 2
	    if (r2 < r2min) {
		r2min = r2
		x2 = x1
		y2 = y1
		nearest = i
	    }
	}

	# Move the cursor to the selected point and mark the deleted point.
	if (nearest > 0) {
	    Memi[flag+nearest] = ALWAYSYES
	    call gscur (gp, x2, y2)
	    call gseti (gp, G_PMLTYPE, 0)
	    y2 = Memr[CR_RATIO(cr)+nearest-1]
	    call gmark (gp, x2, y2, GM_PLUS, 2., 2.)
	    call gseti (gp, G_PMLTYPE, 1)
	    call gmark (gp, x2, y2, GM_CROSS, 2., 2.)
	}
end


# CR_UNDELETE -- Set no replace flag for cosmic ray candidate nearest cursor.

procedure cr_undelete (gp, wx, wy, cr, nearest)

pointer	gp		# GIO pointer
real	wx, wy		# Cursor position
pointer	cr		# Cosmic ray list
int	nearest		# Index of nearest point (returned)

int	i, ncr
real	x0, y0, x1, y1, x2, y2, r2, r2min
pointer	x, y, flag

begin
	ncr = CR_NCR(cr)
	x = CR_FLUX(cr) - 1
	y = CR_RATIO(cr) - 1
	flag = CR_FLAG(cr) - 1

	# Search for nearest point in NDC.
	nearest = 0
	r2min = MAX_REAL
	call gctran (gp, wx, wy, wx, wy, 1, 0)
	do i = 1, ncr {
	    if ((Memi[flag+i] == NO) || (Memi[flag+i] == ALWAYSNO))
		next
	    x1 = Memr[x+i]
	    y1 = Memr[y+i]
	    call gctran (gp, x1, y1, x0, y0, 1, 0)
	    r2 = (x0 - wx) ** 2 + (y0 - wy) ** 2
	    if (r2 < r2min) {
		r2min = r2
		x2 = x1
		y2 = y1
		nearest = i
	    }
	}

	# Move the cursor to the selected point and mark the delete point.
	if (nearest > 0) {
	    Memi[flag+nearest] = ALWAYSNO
	    call gscur (gp, x2, y2)

	    call gseti (gp, G_PMLTYPE, 0)
	    y2 = Memr[CR_RATIO(cr)+nearest-1]
	    call gmark (gp, x2, y2, GM_CROSS, 2., 2.)
	    call gseti (gp, G_PMLTYPE, 1)
	    call gmark (gp, x2, y2, GM_PLUS, 2., 2.)
	}
end


# CR_UPDATE -- Change replacement flags, thresholds, and graphs.

procedure cr_update (gp, wy, cr, fluxratio)

pointer	gp		# GIO pointer
real	wy		# Y cursor position
pointer	cr		# Cosmic ray list
real	fluxratio	# Flux ratio threshold

int	i, ncr, flag
real	x1, x2, y1, y2
pointer	x, y, f

begin
	call gseti (gp, G_PLTYPE, 0)
	call ggwind (gp, x1, x2, y1, y2)
	call gline (gp, x1, fluxratio, x2, fluxratio)
	fluxratio = wy
	call gseti (gp, G_PLTYPE, 2)
	call gline (gp, x1, fluxratio, x2, fluxratio)

	ncr = CR_NCR(cr)
	x = CR_FLUX(cr) - 1
	y = CR_RATIO(cr) - 1
	f = CR_FLAG(cr) - 1

	do i = 1, ncr {
	    flag = Memi[f+i]
	    if ((flag == ALWAYSYES) || (flag == ALWAYSNO))
		next
	    x1 = Memr[x+i]
	    y1 = Memr[y+i]
	    if (flag == NO) {
	        if (y1 < fluxratio) {
		    Memi[f+i] = YES
		    call gseti (gp, G_PMLTYPE, 0)
		    call gmark (gp, x1, y1, GM_PLUS, 2., 2.)
		    call gseti (gp, G_PMLTYPE, 1)
		    call gmark (gp, x1, y1, GM_CROSS, 2., 2.)
		}
	    } else {
	        if (y1 >= fluxratio) {
		    Memi[f+i] = NO
		    call gseti (gp, G_PMLTYPE, 0)
		    call gmark (gp, x1, y1, GM_CROSS, 2., 2.)
		    call gseti (gp, G_PMLTYPE, 1)
		    call gmark (gp, x1, y1, GM_PLUS, 2., 2.)
		}
	    }
	}
end


# CR_PLOT -- Make log plot

procedure cr_plot (cr, im, fluxratio)

pointer	cr			# Cosmic ray list
pointer	im			# Image pointer
real	fluxratio		# Flux ratio threshold

int	fd, open(), errcode()
pointer	sp, fname, gp, gt, gopen(), gt_init()
errchk	gopen

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)

	# Open the plotfile.
	call clgstr ("plotfile", Memc[fname], SZ_FNAME)
	iferr (fd = open (Memc[fname], APPEND, BINARY_FILE)) {
	    if (errcode() != SYS_FNOFNAME)
		call erract (EA_WARN)
	    return
	}

	# Set up the graphics.
	gp = gopen ("stdplot", NEW_FILE, fd)
	gt = gt_init()
	call gt_sets (gt, GTTYPE, "mark")
	call gt_sets (gt, GTXTRAN, "log")
	call gt_setr (gt, GTXMIN, 10.)
	call gt_setr (gt, GTYMIN, 0.)
	call gt_sets (gt, GTTITLE, "Parameters of cosmic rays candidates")
	call gt_sets (gt, GTPARAMS, IM_TITLE(im))
	call gt_sets (gt, GTXLABEL, "Flux")
	call gt_sets (gt, GTYLABEL, "Flux Ratio")

	call cr_graph (gp, gt, cr, fluxratio)

	call gt_free (gt)
	call gclose (gp)
	call close (fd)
	call sfree (sp)
end
