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

define	HELP	"crutil$src/cosmicrays.key"
define	PROMPT	"cosmic ray options"

# CR_EXAMINE -- Examine cosmic ray candidates interactively.

procedure cr_examine (cr, gp, gt, im, fluxratio, first)

pointer	cr			# Cosmic ray list
pointer	gp			# GIO pointer
pointer	gt			# GTOOLS pointer
pointer	im			# Image pointer
real	fluxratio		# Flux ratio threshold
int	first			# Initial key

char	cmd[SZ_LINE]
int	i, newgraph, wcs, key, nc, nl, c1, c2, l1, l2, show
real	wx, wy, x1, y1, x2, y2
pointer	data

int	clgcur()
pointer	imgs2r()

begin
	# Set up the graphics.
	call gt_sets (gt, GTPARAMS, IM_TITLE(im))

	# Set image limits
	nc = IM_LEN(im, 1)
	nl = IM_LEN(im, 2)

	# Enter cursor loop.
	key = first
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
	    case 'a': # Toggle show all
		if (show == 0)
		    show = 1
		else
		    show = 0
		newgraph = YES
	    case 'd': # Delete candidate
		call cr_delete (gp, wx, wy, cr, i, show)
	    case 'e': # Delete candidates in region
                x1 = wx; y1 = wy
                call printf ("again:")
                if (clgcur ("cursor", x2, y2, wcs, key, cmd, SZ_LINE) == EOF)
                    return
		call cr_delete_reg (gp, x1, y1, x2, y2, cr, show)
	    case 'q': # Quit
		break
	    case 'r': # Redraw the graph.
		newgraph = YES
	    case 's': # Make surface plots
		call cr_nearest (gp, wx, wy, cr, i, show)
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
		call cr_update (gp, wy, cr, fluxratio, show)
		call clputr ("fluxratio", fluxratio)
	    case 'u': # Undelete candidate
		call cr_undelete (gp, wx, wy, cr, i, show)
	    case 'v': # Undelete candidates in region
                x1 = wx; y1 = wy
                call printf ("again:")
                if (clgcur ("cursor", x2, y2, wcs, key, cmd, SZ_LINE) == EOF)
                    return
		call cr_undelete_reg (gp, x1, y1, x2, y2, cr, show)
	    case 'w':# Window the graph.
		call gt_window (gt, gp, "cursor", newgraph)
	    case ' ': # Print info
		call cr_nearest (gp, wx, wy, cr, i, show)
		call printf ("%d %d\n")
		    call pargr (Memr[CR_COL(cr)+i-1])
		    call pargr (Memr[CR_LINE(cr)+i-1])
	    case 'z': # NOP
		newgraph = NO
	    default: # Ring bell for unrecognized commands.
		call printf ("\007")
	    }

	    # Update the graph if needed.
	    if (newgraph == YES) {
		call cr_graph (gp, gt, cr, fluxratio, show)
	        newgraph = NO
	    }
	} until (clgcur ("cursor", wx, wy, wcs, key, cmd, SZ_LINE) == EOF)
end


# CR_GRAPH -- Make a graph

procedure cr_graph (gp, gt, cr, fluxratio, show)

pointer	gp		# GIO pointer
pointer	gt		# GTOOLS pointers
pointer	cr		# Cosmic ray list
real	fluxratio	# Flux ratio threshold
int	show		# Show (0=all, 1=train)

int	i, ncr
real	x1, x2, y1, y2
pointer	sp, x, y, w, flag, index

begin
	call smark (sp)

	call cr_show (show, cr, x, y, w, flag, index, ncr)
	if (ncr == 0) {
	    call sfree (sp)
	    return
	}

	call gclear (gp)
	call gt_ascale (gp, gt, Memr[x+1], Memr[y+1], ncr)
	call gt_swind (gp, gt)
	call gt_labax (gp, gt)

	do i = 1, ncr {
	    if ((Memi[flag+i] == NO) || (Memi[flag+i] == ALWAYSNO))
		call gmark (gp, Memr[x+i], Memr[y+i], GM_PLUS, 2., 2.)
	    else
		call gmark (gp, Memr[x+i], Memr[y+i], GM_CROSS, 2., 2.)
	    if (Memr[w+i] != 0.)
		call gmark (gp, Memr[x+i], Memr[y+i], GM_BOX, 2., 2.)
	}

	call ggwind (gp, x1, x2, y1, y2)
	call gseti (gp, G_PLTYPE, 2)
	call gline (gp, x1, fluxratio, x2, fluxratio)

	call sfree (sp)
end


# CR_NEAREST -- Find the nearest cosmic ray to the cursor.

procedure cr_nearest (gp, wx, wy, cr, nearest, show)

pointer	gp		# GIO pointer
real	wx, wy		# Cursor position
pointer	cr		# Cosmic ray list
int	nearest		# Index of nearest point (returned)
int	show		# Show (0=all, 1=train)

int	i, ncr
real	x0, y0, x1, y1, x2, y2, r2, r2min
pointer	sp, x, y, w, flag, index

begin
	call smark (sp)

	call cr_show (show, cr, x, y, w, flag, index, ncr)
	if (ncr == 0) {
	    call sfree (sp)
	    return
	}

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
	if (index != NULL)
	    nearest = Memi[index+nearest]

	# Move the cursor to the selected point.
	call gscur (gp, x2, y2)

	call sfree (sp)
end


# CR_DELETE -- Set replace flag for cosmic ray candidate nearest cursor.

procedure cr_delete (gp, wx, wy, cr, nearest, show)

pointer	gp		# GIO pointer
real	wx, wy		# Cursor position
pointer	cr		# Cosmic ray list
int	nearest		# Index of nearest point (returned)
int	show		# Show (0=all, 1=train)

int	i, ncr
real	x0, y0, x1, y1, x2, y2, r2, r2min
pointer	sp, x, y, w, flag, index

begin
	call smark (sp)

	call cr_show (show, cr, x, y, w, flag, index, ncr)
	if (ncr == 0) {
	    call sfree (sp)
	    return
	}

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
	    if (index != NULL)
		nearest = Memi[index+nearest]
	    Memi[CR_FLAG(cr)+nearest-1] = ALWAYSYES
	    Memr[CR_WT(cr)+nearest-1] = -1
	    call gscur (gp, x2, y2)
	    call gseti (gp, G_PMLTYPE, 0)
	    y2 = Memr[CR_RATIO(cr)+nearest-1]
	    call gmark (gp, x2, y2, GM_PLUS, 2., 2.)
	    call gseti (gp, G_PMLTYPE, 1)
	    call gmark (gp, x2, y2, GM_CROSS, 2., 2.)
	}

	call sfree (sp)
end


# CR_DELETE_REG -- Set replace flag for cosmic ray candidates in a region.

procedure cr_delete_reg (gp, wx1, wy1, wx2, wy2, cr, show)

pointer	gp			# GIO pointer
real	wx1, wy1, wx2, wy2	# Cursor positions
pointer	cr			# Cosmic ray list
int	show			# Show (0=all, 1=train)

int	i, j, ncr
real	x0, y0, x1, y1
pointer	sp, x, y, w, flag, index

begin
	call smark (sp)

	call cr_show (show, cr, x, y, w, flag, index, ncr)
	if (ncr == 0) {
	    call sfree (sp)
	    return
	}

	# Check order of region points.
	if (wx1 > wx2) {
	    x0 = wx1
	    wx1 = wx2
	    wx2 = x0
	}
	if (wy1 > wy2) {
	    y0 = wy1
	    wy1 = wy2
	    wy2 = y0
	}

	# Check if point in region.
	call gctran (gp, wx1, wy1, wx1, wy1, 1, 0)
	call gctran (gp, wx2, wy2, wx2, wy2, 1, 0)
	do i = 1, ncr {
	    if ((Memi[flag+i] == YES) || (Memi[flag+i] == ALWAYSYES))
		next
	    x1 = Memr[x+i]
	    y1 = Memr[y+i]
	    call gctran (gp, x1, y1, x0, y0, 1, 0)

	    # Mark the deleted points.
	    if ((x0 > wx1) && (x0 < wx2) && (y0 > wy1) && (y0 < wy2)) {
		if (index != NULL)
		    j = Memi[index+i]
		else
		    j = i
		Memi[CR_FLAG(cr)+j-1] = ALWAYSYES
		Memr[CR_WT(cr)+j-1] = -1
		call gscur (gp, x1, y1)
		call gseti (gp, G_PMLTYPE, 0)
		y1 = Memr[CR_RATIO(cr)+j-1]
		call gmark (gp, x1, y1, GM_PLUS, 2., 2.)
		call gseti (gp, G_PMLTYPE, 1)
		call gmark (gp, x1, y1, GM_CROSS, 2., 2.)
	    }
	}
	call sfree (sp)
end


# CR_UNDELETE -- Set no replace flag for cosmic ray candidate nearest cursor.

procedure cr_undelete (gp, wx, wy, cr, nearest, show)

pointer	gp		# GIO pointer
real	wx, wy		# Cursor position
pointer	cr		# Cosmic ray list
int	nearest		# Index of nearest point (returned)
int	show		# Show (0=all, 1=train)

int	i, ncr
real	x0, y0, x1, y1, x2, y2, r2, r2min
pointer	sp, x, y, w, flag, index

begin
	call smark (sp)

	call cr_show (show, cr, x, y, w, flag, index, ncr)
	if (ncr == 0) {
	    call sfree (sp)
	    return
	}

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
	    if (index != NULL)
		nearest = Memi[index+nearest]
	    Memi[CR_FLAG(cr)+nearest-1] = ALWAYSNO
	    Memr[CR_WT(cr)+nearest-1] = 1
	    call gscur (gp, x2, y2)

	    call gseti (gp, G_PMLTYPE, 0)
	    y2 = Memr[CR_RATIO(cr)+nearest-1]
	    call gmark (gp, x2, y2, GM_CROSS, 2., 2.)
	    call gseti (gp, G_PMLTYPE, 1)
	    call gmark (gp, x2, y2, GM_PLUS, 2., 2.)
	}

	call sfree (sp)
end


# CR_UNDELETE_REG -- Set no replace flag for cosmic ray candidates in a region.

procedure cr_undelete_reg (gp, wx1, wy1, wx2, wy2, cr, show)

pointer	gp			# GIO pointer
real	wx1, wy1, wx2, wy2	# Cursor positions
pointer	cr			# Cosmic ray list
int	show			# Show (0=all, 1=train)

int	i, j, ncr
real	x0, y0, x1, y1
pointer	sp, x, y, w, flag, index

begin
	call smark (sp)

	call cr_show (show, cr, x, y, w, flag, index, ncr)
	if (ncr == 0) {
	    call sfree (sp)
	    return
	}

	# Check order of region points.
	if (wx1 > wx2) {
	    x0 = wx1
	    wx1 = wx2
	    wx2 = x0
	}
	if (wy1 > wy2) {
	    y0 = wy1
	    wy1 = wy2
	    wy2 = y0
	}

	# Check if point in region.
	call gctran (gp, wx1, wy1, wx1, wy1, 1, 0)
	call gctran (gp, wx2, wy2, wx2, wy2, 1, 0)
	do i = 1, ncr {
	    if ((Memi[flag+i] == NO) || (Memi[flag+i] == ALWAYSNO))
		next
	    x1 = Memr[x+i]
	    y1 = Memr[y+i]
	    call gctran (gp, x1, y1, x0, y0, 1, 0)

	    # Mark the deleted points.
	    if ((x0 > wx1) && (x0 < wx2) && (y0 > wy1) && (y0 < wy2)) {
		if (index != NULL)
		    j = Memi[index+i]
		else
		    j = i
		Memi[CR_FLAG(cr)+j-1] = ALWAYSNO
		Memr[CR_WT(cr)+j-1] = 1
		call gscur (gp, x1, y1)
		call gseti (gp, G_PMLTYPE, 0)
		y1 = Memr[CR_RATIO(cr)+j-1]
		call gmark (gp, x1, y1, GM_CROSS, 2., 2.)
		call gseti (gp, G_PMLTYPE, 1)
		call gmark (gp, x1, y1, GM_PLUS, 2., 2.)
	    }
	}
	call sfree (sp)
end


# CR_UPDATE -- Change replacement flags, thresholds, and graphs.

procedure cr_update (gp, wy, cr, fluxratio, show)

pointer	gp		# GIO pointer
real	wy		# Y cursor position
pointer	cr		# Cosmic ray list
real	fluxratio	# Flux ratio threshold
int	show		# Show (0=all, 1=train)

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

	if (show == 1)
	    return

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

	call cr_graph (gp, gt, cr, fluxratio, 'r')

	call gt_free (gt)
	call gclose (gp)
	call close (fd)
	call sfree (sp)
end


# CR_SHOW -- Select data to show.
# This returns pointers to the data.  Note the pointers are salloc from
# the last smark which is done by the calling program.

procedure cr_show (show, cr, x, y, w, flag, index, ncr)

int	show		#I Data to show (0=all, 1=train)
pointer	cr		#I CR data
pointer	x		#O Fluxes
pointer	y		#O Ratios
pointer	w		#O Weights
pointer	flag		#O Flags
pointer	index		#O Index into CR data (if not null)
int	ncr		#O Number of selected data points

int	i

begin
	switch (show) {
	case 0:
	    ncr = CR_NCR(cr)
	    x = CR_FLUX(cr) - 1
	    y = CR_RATIO(cr) - 1
	    w = CR_WT(cr) - 1
	    flag = CR_FLAG(cr) - 1
	    index = NULL
	case 1:
	    ncr = CR_NCR(cr)
	    call salloc (x, ncr, TY_REAL)
	    call salloc (y, ncr, TY_REAL)
	    call salloc (w, ncr, TY_REAL)
	    call salloc (flag, ncr, TY_INT)
	    call salloc (index, ncr, TY_INT)

	    ncr = 0
	    x = x - 1
	    y = y - 1
	    w = w - 1
	    flag = flag - 1
	    index = index - 1

	    do i = 1, CR_NCR(cr) {
		if (Memr[CR_WT(cr)+i-1] == 0.)
		    next
		ncr = ncr + 1
		Memr[x+ncr] = Memr[CR_FLUX(cr)+i-1]
		Memr[y+ncr] = Memr[CR_RATIO(cr)+i-1]
		Memr[w+ncr] = Memr[CR_WT(cr)+i-1]
		Memi[flag+ncr] = Memi[CR_FLAG(cr)+i-1]
		Memi[index+ncr] = i
	    }
	}
end
