include	<imhdr.h>
include	<gset.h>
include	<pkg/rg.h>
include	<pkg/gtools.h>
include	<pkg/xtanswer.h>

define	HELP		"noao$lib/scr/ilsetbins.key"
define	PROMPT		"illumination options"
define	SZ_BINS		2048	# Length of bin string

# IL_SETBINS -- Set the dispersion bins.

procedure il_setbins (im, axis, interactive, rg)

pointer	im			# IMIO pointer for calibration image
int	axis			# Slit axis
int	interactive		# Set bins interactively?
pointer	rg			# Range pointer for bins

char	bins[SZ_BINS], str[SZ_LINE]
int	i, npts, nbins
real	dx
pointer x

int	clgeti()
pointer	rg_ranges()

begin
	# Get the bins.  If the bin string is null then divide the dispersion
	# range into a number of equal bins.

	call clgstr ("bins", bins, SZ_BINS)
	call xt_stripwhite (bins)

	npts = IM_LEN (im, axis)

	if (bins[1] == EOS) {
	    call malloc (x, npts, TY_INT)
	    do i = 1, npts
		Memi[x+i-1] = i
	    nbins = clgeti ("nbins")
	    dx = npts / nbins
	    do i = 1, nbins {
		call sprintf (str, SZ_LINE, "%d:%d ")
		    call pargi (Memi[x + int ((i - 1) * dx)])
		    call pargi (Memi[x + int (i * dx - 1)])
		call strcat (str, bins, SZ_BINS)
	    }
	    call mfree (x, TY_INT)
	}

	rg = rg_ranges (bins, 1, npts)
	if (rg == NULL)
	    call error (0, "Bad range string for parameter bins")

	# Set the bins interactively.

	if ((interactive == YES) || (interactive == ALWAYSYES)) {
	    call sprintf (str, SZ_LINE, "Set illumination bins\n%s")
		call pargstr (IM_TITLE(im))
	    call il_gsetbins (im, axis, str, bins, SZ_BINS, rg)
	}

	call rg_order (rg)
end


# IL_GSETBINS -- Set dispersion bins graphically.

procedure il_gsetbins (im, axis, title, bins, sz_bins, rg)

pointer	im			# IMIO pointer
int	axis			# Slit axis
char	title[ARB]		# Title
char	bins[sz_bins]		# Bin string
int	sz_bins			# Size of bin string
pointer	rg			# Range pointer for the bins

int	npts, newbins, newgraph
real	x1, x2
char	oldbins[SZ_BINS]
pointer	gp, gt, x, y

real	wx, wy
int	wcs, key
char	cmd[SZ_BINS]

int	gt_gcur(), stridxs(), strlen()
pointer	gopen(), gt_init(), rg_xrangesr()

begin
	# Get the average spectrum.

	call ls_aimavg (im, axis, 1, IM_LEN(im,1), 1, IM_LEN(im,2), x, y, npts)

	# Graph the spectrum and mark the bins.

	call clgstr ("graphics", oldbins, SZ_BINS)
	gp = gopen (oldbins, NEW_FILE, STDGRAPH)
	gt = gt_init()
	call il_gbins (gp, gt, axis, Memr[x], Memr[y], npts, bins, title)

	while (gt_gcur ("cursor", wx, wy, wcs, key, cmd, SZ_BINS) != EOF) {
	    switch (key) {
	    case '?': # Print help text
		call gpagefile (gp, HELP, PROMPT)

	    case ':': # Colon commands
		call strcpy (bins, oldbins, SZ_BINS)
		if (cmd[1] == '/')
		    call gt_colon (cmd, gp, gt, newgraph)
		else
		    call il_colon (cmd, bins, sz_bins, newbins)
		if (newgraph == YES) {
		    call il_gbins (gp, gt, axis, Memr[x], Memr[y], npts, bins,
			title)
		} else if (newbins == YES) {
		    call rg_gxmarkr (gp, oldbins, Memr[x], npts, 0)
		    call rg_gxmarkr (gp, bins, Memr[x], npts, 1)
		}

	    case 'i': # Initialize range string
		call rg_gxmarkr (gp, bins, Memr[x], npts, 0)
		call sprintf (bins, sz_bins, "*")

	    case 's': # Set sample ranges with the cursor.
		if (stridxs ("*", bins) > 0)
		    bins[1] = EOS

		x1 = wx
		call printf ("again:\n")
		if (gt_gcur ("cursor", wx, wy, wcs, key, cmd, SZ_BINS) == EOF)
		    break

		x2 = wx
		call sprintf (cmd, SZ_BINS, "%d:%d ")
		    call pargr (x1)
		    call pargr (x2)
		if (strlen (cmd) + strlen (bins) > sz_bins)
		    call eprintf (
			"Warning: Too many bins.  New bin ignored.\n")
		else {
		    call strcat (cmd, bins, sz_bins)
		    call rg_gxmarkr (gp, bins, Memr[x], npts, 1)
		}

	    case 'I':
		call fatal (0, "Interrupt")

	    default: # Ring bell for unrecognized commands.
		call printf ("\7\n")
	    }
	}

	rg = rg_xrangesr (bins, Memr[x], npts)

	call mfree (x, TY_REAL)
	call mfree (y, TY_REAL)
	call gclose (gp)
	call gt_free (gt)
end


define	COMMANDS "|show|bins|"
define	SHOW		1	# Show bins
define	BINS		2	# Set bins

# IL_COLON -- Processes colon commands.

procedure il_colon (cmdstr, bins, sz_bins, newbins)

char	cmdstr[ARB]			# Colon command
char	bins[sz_bins]			# Bins string
int	sz_bins				# Size of bins string
int	newbins				# New bins?

char	cmd[SZ_BINS]
int	ncmd

int	strdic()

begin
	newbins = NO

	call sscan (cmdstr)
	call gargwrd (cmd, SZ_BINS)
	ncmd = strdic (cmd, cmd, SZ_BINS, COMMANDS)

	switch (ncmd) {
	case SHOW:
	    call printf ("bins = %s\n")
		call pargstr (bins)
	case BINS:
	    call gargstr (cmd, SZ_BINS)
	    call xt_stripwhite (cmd)
	    if (cmd[1] == EOS) {
		call printf ("bins = %s\n")
		    call pargstr (bins)
	    } else {
		call strcpy (cmd, bins, sz_bins)
		newbins = YES
	    }
	}
end


# IL_GBINS -- Graph data

procedure il_gbins (gp, gt, axis, x, y, npts, bins, title)

pointer	gp			# GIO pointer
pointer	gt			# GTOOLS pointer
int	axis			# Slit axis
real	x[npts], y[npts]	# Data to graph
int	npts			# Number of data points
char	bins[ARB]		# Bins to graph
char	title[ARB] 		# Graph labels

begin
	call gclear (gp)
	call gascale (gp, x, npts, 1)
	call gascale (gp, y, npts, 2)
	call gt_swind (gp, gt)
	switch (axis) {
	case 1:
	    call glabax (gp, title, "Line", "")
	case 2:
	    call glabax (gp, title, "Column", "")
	}
	call gpline (gp, x, y, npts)
	call rg_gxmarkr (gp, bins, x, npts, 1)
end
