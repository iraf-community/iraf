# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<xwhen.h>
include	<config.h>
include	<imhdr.h>
include	<mach.h>
include	<error.h>
include	<ctype.h>
include	<fset.h>
include	<gset.h>
include	<mwset.h>

define	SZ_BUF		2048		# Initial pixel buffer size
define	MAX_CURVES	20		# maximum curves if overplotting
define	LIST_OP		1
define	IMAGE_OP	2


# GRAPH -- Graphing utility where input may be one or more lists (y or x,y) 
# or image sections.  Multidimensional image sections are reduced to a vector 
# by computing the projection about the indicated axis.  Many options are 
# available to personalize the plot; see the manual page for a full description.

procedure t_graph()

char	input[SZ_LINE]
pointer	x[MAX_CURVES], y[MAX_CURVES], size[MAX_CURVES]
int	npix[MAX_CURVES], ncurves

bool	append, overplot
char	device[SZ_FNAME]
int	tgrjmp[LEN_JUMPBUF], epa, old_onint, status, i

bool	clgetb()
int	fstati()
extern	tgr_onint()
common	/tgrcom/ tgrjmp

begin
	# Initialize curve pointers to NULL, in case ggplot aborts without
	# allocating any buffers.
	do i = 1, MAX_CURVES {
	    x[i] = NULL
	    y[i] = NULL
	    size[i] = NULL
	    npix[i] = NULL
	}

	if (fstati (STDIN, F_REDIR) == YES)
	    call strcpy ("STDIN", input, SZ_FNAME)
	else
	    call clgstr ("input", input, SZ_LINE)

	# Fetch plotting parameters.

	call clgstr ("device", device, SZ_FNAME)
	overplot = clgetb ("overplot")
	append = clgetb ("append")

	# Install interrupt exception handler.
	call zlocpr (tgr_onint, epa)
	call xwhen (X_INT, epa, old_onint)

	call zsvjmp (tgrjmp, status)
	if (status == OK) {
	    # Fetch remaining params and draw the plot.
	    iferr (call ggplot (device, overplot, append, input, x, y,
		size, npix, ncurves))
		status = ERR
	}

	if (status == ERR)
	    call fseti (STDOUT, F_CANCEL, OK)

	# Return buffer space whether or not an error occurs while plotting.

	do i = 1, MAX_CURVES {
	    call mfree (x[i], TY_REAL)
	    call mfree (y[i], TY_REAL)
	    call mfree (size[i], TY_REAL)
	}

	if (status == ERR)
	    call erract (EA_ERROR)
end


# TGR_ONINT -- Interrupt handler for the task graph.  Branches back to ZSVJMP
# in the main routine to permit shutdown without an error message.

procedure tgr_onint (vex, next_handler)

int	vex			# Virtual exception
int	next_handler		# not used

int	tgrjmp[LEN_JUMPBUF]
common	/tgrcom/ tgrjmp

begin
	call xer_reset()
	call zdojmp (tgrjmp, vex)
end


# GGPLOT -- Does the real work of making the graph, after the graphics
# devics has been opened.  Fetch remaining parameters, read in the data,
# and make the plot.

procedure ggplot (device, overplot, append, input, x, y, size, npix, ncurves)

char	device[SZ_FNAME]	# Graphics device
bool	overplot		# Overplot graph
bool	append			# Append graph
char	input[ARB]		# List of operands to be plotted
pointer	x[MAX_CURVES]		# X values
pointer y[MAX_CURVES]		# Y values
pointer size[MAX_CURVES]	# Size of markers to plot
int	npix[MAX_CURVES]	# Number of points per curve
int	ncurves			# Number of curves to overplot

pointer	gd
char	xlabel[SZ_LINE], ylabel[SZ_LINE], title[SZ_LINE]
char	marker[SZ_FNAME], wcs[SZ_FNAME], xformat[SZ_FNAME], yformat[SZ_FNAME]
bool	pointmode, lintran, xautoscale, yautoscale
bool	drawbox, transpose, rdmarks
int	ltype, color, ip1, ip2
int	xtran, ytran, axis, ticklabels, i, marker_type, j
real	p1, p2, q1, q2, wx1, wx2, wy1, wy2, szmarker, vx1, vx2, vy1, vy2
real	xx, yy, sz, szx, szy
pointer	sp, ltypes, colors, ptemp

pointer	gopen()
bool	clgetb(), streq(), fp_equalr()
int	clgeti(), gg_rdcurves(), ctoi(), gstati()
real	clgetr(), plt_iformatr()
errchk	clgetb, clgeti, clgstr, clgetr, glabax, gpmark
errchk	gswind, gseti, gg_rdcurves, gascale, grscale

begin
	call smark (sp)
	call salloc (ltypes, SZ_LINE, TY_CHAR)
	call salloc (colors, SZ_LINE, TY_CHAR)

	# If computing projection along an axis (collapsing a multidimensional
	# section to a vector), fetch axis number.  Get wcs string.
	axis = clgeti ("axis")
	call clgstr ("wcs", wcs, SZ_FNAME)

	# Set the line type and color lists.
	i = 0
	call clgstr ("ltypes", Memc[ltypes], SZ_LINE)
	for (ip1=ltypes; Memc[ip1]!=EOS; ip1=ip1+1) {
	    if (Memc[ip1] == ',')
		Memc[ip1] = ' '
	    if (IS_DIGIT(Memc[ip1]))
		i = i + 1
	}
	if (i == 0)
	    Memc[ltypes] = EOS
	ip1 = 1
	ltype = 0

	i = 0
	call clgstr ("colors", Memc[colors], SZ_LINE)
	for (ip2=colors; Memc[ip2]!=EOS; ip2=ip2+1) {
	    if (Memc[ip2] == ',')
		Memc[ip2] = ' '
	    if (IS_DIGIT(Memc[ip2]))
		i = i + 1
	}
	if (i == 0)
	    Memc[colors] = EOS
	ip2 = 1
	color = 0

	# If pointmode is enabled, get marker character to be used to mark
	# points.  The size of the character is given
	# by szmarker; if zero, the size will be taken from the input list.

	pointmode = clgetb ("pointmode")
	szmarker = 0.0
	rdmarks = false

	if (pointmode) {
	    call clgstr ("marker", marker, SZ_FNAME)
	    call init_marker (marker, marker_type)
	    if (marker_type != GM_POINT) {
		szmarker = clgetr ("szmarker")
		rdmarks = (szmarker <= 0)
	    }
	} else
	    call clgstr ("marker", marker, SZ_FNAME)

	# Read all the curves specified by the operands in input into memory.
	# Get the first image title and coordinate label.

	title[1] = EOS
	xlabel[1] = EOS
	ylabel[1] = EOS
	xformat[1] = EOS
	yformat[1] = EOS
	ncurves = gg_rdcurves (input, title, xlabel, ylabel, xformat,
	    x, y, size, npix, axis, wcs, rdmarks)

	if (overplot || append)
	    gd = gopen (device, APPEND, STDGRAPH)
	else
	    gd = gopen (device, NEW_FILE, STDGRAPH)

	xautoscale = false
	yautoscale = false

	# Set window and viewport.  If user window has not been set, enable
	# autoscaling.  If device viewport has not been set, let glabax
	# handle the viewport internally.

	if (!append) {
	    wx1 = clgetr ("wx1")
	    wx2 = clgetr ("wx2")
	    wy1 = clgetr ("wy1")
	    wy2 = clgetr ("wy2")

	    if (fp_equalr (wx1, wx2))
		xautoscale = true
	    if (fp_equalr (wy1, wy2))
		yautoscale = true

	    vx1 = clgetr ("vx1")
	    vx2 = clgetr ("vx2")
	    vy1 = clgetr ("vy1")
	    vy2 = clgetr ("vy2")

	    if (!(fp_equalr (vx1, vx2)) && !(fp_equalr (vy1, vy2)))
	        call gsview (gd, vx1, vx2, vy1, vy2)

	    if (!clgetb ("fill"))
	        call gseti (gd, G_ASPECT, 1)

	    if (clgetb ("round"))
	        call gseti (gd, G_ROUND, YES)
	}

	# Draw box around plot?
	drawbox = false
	if (!append)
	    if (clgetb ("box"))
		drawbox = true

	if (drawbox) {
	    # Get number of major and minor tick marks.
	    call gseti (gd, G_XNMAJOR, clgeti ("majrx"))
	    call gseti (gd, G_XNMINOR, clgeti ("minrx"))
	    call gseti (gd, G_YNMAJOR, clgeti ("majry"))
	    call gseti (gd, G_YNMINOR, clgeti ("minry"))

	    # Fetch plot title, labels and format
	    call clgstr ("title", wcs, SZ_LINE)
	    if (!streq (wcs, "imtitle"))
		call strcpy (wcs, title, SZ_LINE)

	    call clgstr ("xlabel", wcs, SZ_LINE)
	    if (!streq (wcs, "wcslabel"))
		call strcpy (wcs, xlabel, SZ_LINE)

	    call clgstr ("ylabel", ylabel, SZ_LINE)

	    call clgstr ("xformat", wcs, SZ_LINE)
	    if (!streq (wcs, "wcsformat"))
		call strcpy (wcs, xformat, SZ_FNAME)

	    call clgstr ("yformat", yformat, SZ_LINE)

	    # Label tick marks on axes?
	    ticklabels = NO
	    if (clgetb ("ticklabels"))
		ticklabels = YES
	}

	# Perform linear transformation on the X axis?
	lintran = clgetb ("lintran")
	if (lintran) {
	    p1 = clgetr ("p1")
	    p2 = clgetr ("p2")
	    q1 = clgetr ("q1")
	    q2 = clgetr ("q2")
	}

	# Transpose X,Y axes?
	transpose = clgetb ("transpose")

	# Log scale?  Call gswind to set log scaling regardless of whether
	# the user window is known; if the user window was not input,
	# autoscaling will reset it later.

	if (append) {
	    xtran = gstati (gd, G_XTRAN)
	    ytran = gstati (gd, G_YTRAN)
	    call ggwind (gd, wx1, wx2, wy1, wy2)
	} else {
	    xtran = GW_LINEAR
	    if (clgetb ("logx"))
		xtran = GW_LOG
	    ytran = GW_LINEAR
	    if (clgetb ("logy"))
		ytran = GW_LOG
	    wx1 = plt_iformatr (wx1, xformat)
	    wx2 = plt_iformatr (wx2, xformat)
	    wy1 = plt_iformatr (wy1, yformat)
	    wy2 = plt_iformatr (wy2, yformat)
	    call gswind (gd, wx1, wx2, wy1, wy2)
	    call gseti (gd, G_XTRAN, xtran)
	    call gseti (gd, G_YTRAN, ytran)
	}

	# Carry out linear transformation on X coords, if desired.
	if (lintran)
	    do i = 1, ncurves
		call gg_lintran (Memr[x[i]], npix[i], p1,p2, q1,q2)

	# Swap axes, if enabled.  Note that the linear transformation of
	# the x-axis should be performed before axes are swapped.  This is
	# because the purpose of the lintran option is to provide a means
	# of assigning a coordinate system to a pixel array.

	if (transpose)
	    do i = 1, ncurves {
		ptemp = x[i]
		x[i] = y[i]
		y[i] = ptemp
	    }

	# Autoscale if enabled.
	if (xautoscale) {
	    call gascale (gd, Memr[x[1]], npix[1], 1)
	    if (ncurves > 1) {
		do i = 2, ncurves
		    call grscale (gd, Memr[x[i]], npix[i], 1)
	    }
	} 

	if (yautoscale) {
	    call gascale (gd, Memr[y[1]], npix[1], 2)
	    if (ncurves > 1) {
		do i = 2, ncurves
		    call grscale (gd, Memr[y[i]], npix[i], 2)
	    }
	}

	# Draw box around plot if enabled.
	if (drawbox) {
	    call gsets (gd, G_XTICKFORMAT, xformat)
	    call gsets (gd, G_YTICKFORMAT, yformat)
	    call gseti (gd, G_LABELTICKS, ticklabels)
	    call glabax (gd, title, xlabel, ylabel)
	}

	# Draw the curves.
	do i = 1, ncurves {
	    if (Memc[ltypes] == EOS)
		ltype = ltype + 1
	    else if (ctoi (Memc[ltypes], ip1, j) > 0)
		ltype = j
	    ltype = mod (ltype - 1, 4) + 1
	    call gseti (gd, G_PLTYPE, ltype)
	    if (Memc[colors] == EOS)
		color = color + 1
	    else if (ctoi (Memc[colors], ip2, j) > 0)
		color = j
	    color = mod (color - 1, 9) + 1
	    call gseti (gd, G_PLCOLOR, color)
	    if (pointmode) {
		if (!rdmarks) {
		    call amovkr (szmarker, Memr[size[i]], npix[i])
		    call gpmark (gd, Memr[x[i]], Memr[y[i]], npix[i], 
		        marker_type, Memr[size[i]], Memr[size[i]])
		} else {
		    if (szmarker < 0)
		        call amulkr (Memr[size[i]], szmarker, Memr[size[i]],
			    npix[i])
		    do j = 1, npix[i] {
			xx = Memr[x[i]+j-1]
			yy = Memr[y[i]+j-1]
			sz = Memr[size[i]+j-1]
			szx= sz; szy = sz
			if (marker_type == GM_VEBAR)
			    szx = 1.0
			else if (marker_type == GM_HEBAR)
			    szy = 1.0
			call gmark (gd, xx, yy, marker_type, szx, szy)
		    }
		}
	    } else
		call hgpline (gd, Memr[x[i]], Memr[y[i]], npix[i], marker)
	}

	call gclose (gd)
	call sfree (sp)
end


# GG_RDCURVES -- Given the operand list as input, read in all the referenced
# lists and/or image sections, producing a list of vectors as output.  Return
# as the function value the number of curves.

int procedure gg_rdcurves (oplist, title, xlabel, ylabel, xformat,
	x, y, size, npix, axis, wcs, rdmarks)

char	oplist[ARB]		# Operand list
char	title[ARB]		# Title
char	xlabel[ARB]		# X label
char	ylabel[ARB]		# Y label
char	xformat[ARB]		# WCS coordinate format
pointer	x[ARB]			# Pointer to x vector
pointer	y[ARB]			# Pointer to y vector
pointer	size[ARB]		# Pointer to vector of marker sizes
int	npix[ARB]		# Number of values per vector
int	axis			# Axis for projection
char	wcs[ARB]		# WCS type
bool	rdmarks			# Read marks from list?

char	operand[SZ_FNAME]
int	ncurves, i, fd
int	gg_rdcurve(), imtopen(), imtgetim()

begin
	ncurves = 0

	# Read all the curves into memory.

	fd = imtopen (oplist)
	while (imtgetim (fd, operand, SZ_FNAME) != EOF) {
	    ncurves = ncurves + 1
	    if (ncurves > MAX_CURVES)
		call error (0, "Maximum of 20 curves can be overplotted")
	    i = ncurves
	    iferr {
		npix[i] = gg_rdcurve (operand, title, xlabel, ylabel,
		    xformat, x[i], y[i], size[i], axis, wcs, rdmarks)
	    } then {
		call erract (EA_WARN)
		ncurves = ncurves - 1
	    }
	}

	call imtclose (fd)

	if (ncurves == 0)
	    call error (0, "No curves read")
	else
	    return (ncurves)
end


# GG_RDCURVE -- Read a curve into memory.  The operand may specify either
# list or image input; we determine which and then call the appropriate
# input routine to access the data.  Set the image title and coordinate
# label if not previously defined.

int procedure gg_rdcurve (operand, title, xlabel, ylabel, xformat,
	x, y, size, axis, wcs, rdmarks)

char	operand[ARB]		# List of operaands to be plotted
char	title[ARB]		# Title
char	xlabel[ARB]		# X label
char	ylabel[ARB]		# Y label
char	xformat[ARB]		# WCS coordinate format
pointer	x, y, size		# Pointers to x, y and size arrays
int	axis			# Axis of image projection
char	wcs[ARB]		# WCS type
bool	rdmarks			# Read marks from list?

int	gg_rdlist2(), gg_rdimage2(), gg_optype()
errchk	gg_rdlist2, gg_rdimage2, gg_optype

begin
	if (gg_optype (operand) == LIST_OP)
	    return (gg_rdlist2 (operand, x, y, size, rdmarks))
	else
	    return (gg_rdimage2 (operand, title, xlabel, ylabel, xformat,
		x, y, size, axis, wcs))
end


# GG_OPTYPE -- Determine whether the operand argument is an image section
# or a list.  If the string is STDIN, it is a list; if a subscript is
# present, it is an image; otherwise we must test whether or not it is a
# binary file and make the decision based on that.

int procedure gg_optype (operand)

char	operand[ARB]		# Operand to be plotted
int	first, last, ip
int	access(), strncmp()

begin
	# Strip off any whitespace at the beginning or end of the string.
	for (ip=1;  IS_WHITE(operand[ip]);  ip=ip+1)
	    ;
	first = ip
	for (last=ip;  operand[ip] != EOS;  ip=ip+1)
	    if (!IS_WHITE(operand[ip]))
		last = ip

	if (first == last)
	    return (LIST_OP)
	else if (strncmp (operand[first], "STDIN", 5) == 0)
	    return (LIST_OP)
	else if (operand[last] == ']')
	    return (IMAGE_OP)
	else if (access (operand, 0, TEXT_FILE) == YES)
	    return (LIST_OP)
	else 
	    return (IMAGE_OP)
end


# GG_RDIMAGE2 -- Read an image section and compute the projection about
# one dimension, producing x and y vectors as output.  Set the title
# and coordinate label if not previously defined.

int procedure gg_rdimage2 (imsect, title, xlabel, ylabel, xformat, x, y, size,
	axis, wcs)

char	imsect[ARB]		# Image section to be plotted
char	title[ARB]		# Image title
char	xlabel[ARB]		# Coordinate label
char	ylabel[ARB]		# Pixel value label
char	xformat[ARB]		# WCS coordinate format
pointer	x, y, size		# Pointer to x, y and size vector
int	axis			# Axis about which the projection is to be taken
char	wcs[ARB]		# WCS type

int	npix, i, stridxs()
pointer	sp, im, mw, ct, axvals, str
pointer	immap(), mw_openim(), mw_sctran()
errchk	immap, im_projection, malloc, mw_openim, mw_sctran, plt_wcs

begin
	call smark (sp)
	call salloc (axvals, IM_MAXDIM, TY_REAL)
	call salloc (str, SZ_FNAME, TY_CHAR)

	im = immap (imsect, READ_ONLY, 0)

	if (axis < 1 || axis > IM_NDIM(im))
	    call error (2, "Attempt to take projection over nonexistent axis")
	npix = IM_LEN(im,axis)

	call malloc (y, npix, TY_REAL)
	call im_projection (im, Memr[y], npix, axis)

	iferr {
	    call malloc (x, npix, TY_REAL)
	    call malloc (size, npix, TY_REAL)
	} then
	    call erract (EA_FATAL)

	# Set title if not previously defined
	if (title[1] == EOS) {
	    call strcpy (IM_TITLE(im), title, SZ_LINE)
	    if (stridxs ("\n", title) == 0)
		call strcat ("\n", title, SZ_LINE)
	    call imgsection (imsect, Memc[str], SZ_LINE)
	    if (Memc[str] != EOS)
		    call strcat (Memc[str], title, SZ_LINE)
	}

	# Set WCS coordinates
	mw = mw_openim (im)
	call mw_seti (mw, MW_USEAXMAP, NO)
	ct = mw_sctran (mw, "logical", wcs, 0)
	call strcpy (wcs, Memc[str], SZ_LINE)
	do i = 1, IM_NDIM(im)
	    Memr[axvals+i-1] = (1 + IM_LEN(im, i)) / 2.
	call plt_wcs (im, mw, ct, axis, Memr[axvals], 1., real(npix), Memr[x],
	    npix, Memc[str], xformat,  SZ_FNAME)
	if (xlabel[1] == EOS)
	    call strcpy (Memc[str], xlabel, SZ_LINE)
	call mw_close (mw)

	call imunmap (im)

	call sfree (sp)
	return (npix)
end


# GG_RDLIST2 -- Read a list of two dimensional data pairs into two type
# real arrays in memory.  Return pointers to the arrays and a count of the
# number of pixels.  If mark sizes are to be read from the input list,
# a third array of mark sizes is returned.  Mark sizes can only be given
# in two column (x,y) mode, with the mark size given as a third column.

int procedure gg_rdlist2 (fname, x, y, size, rdmarks)

char	fname[ARB]		# Name of list file
pointer	x, y, size		# Pointers to x, y and size vectors
bool	rdmarks			# Read markers from file?

int	buflen, n, fd, ncols, lineno
pointer	sp, lbuf, ip
real	xval, yval, szmark
int	getline(), nscan(), open()
errchk	open, sscan, getline, malloc

begin
	call smark (sp)
	call salloc (lbuf, SZ_LINE, TY_CHAR)

	fd = open (fname, READ_ONLY, TEXT_FILE)

	buflen = SZ_BUF
	iferr {
	    call malloc (x, buflen, TY_REAL)
	    call malloc (y, buflen, TY_REAL)
	    call malloc (size, buflen, TY_REAL)
	} then
	    call erract (EA_FATAL)

	n = 0
	ncols = 0
	lineno = 0
	szmark = 1E-2

	while (getline (fd, Memc[lbuf]) != EOF) {
	    # Skip comment lines and blank lines.
	    lineno = lineno + 1
	    if (Memc[lbuf] == '#')
		next
	    for (ip=lbuf;  IS_WHITE(Memc[ip]);  ip=ip+1)
		;
	    if (Memc[ip] == '\n' || Memc[ip] == EOS)
		next

	    # Decode the points to be plotted.
	    call sscan (Memc[ip])
		call gargr (xval)
		call gargr (yval)
		if (rdmarks)
		    call gargr (szmark)

	    # The first line determines whether we have an x,y list or a
	    # y-list.  It is an error if only one value can be decoded when
	    # processing a two column list.

	    if (ncols == 0 && nscan() > 0)
		ncols = nscan()
	    
	    switch (nscan()) {
	    case 0:
		call eprintf ("no args; %s, line %d: %s\n")
		    call pargstr (fname)
		    call pargi (lineno)
		    call pargstr (Memc[lbuf])
		next
	    case 1:
		if (ncols == 2) {
		    call eprintf ("only 1 arg; %s, line %d: %s\n")
			call pargstr (fname)
			call pargi (lineno)
			call pargstr (Memc[lbuf])
		    next
		} else {
		    yval = xval
		    xval = n + 1.0
		}
	    case 2:
		if (rdmarks) {
		    call eprintf ("no szmark field; %s, line %d: %s\n")
			call pargstr (fname)
			call pargi (lineno)
			call pargstr (Memc[lbuf])
		    szmark = 1E-2
		}
	    }

	    n = n + 1
	    if (n > buflen) {
		buflen = buflen + SZ_BUF
		call realloc (x, buflen, TY_REAL)
		call realloc (y, buflen, TY_REAL)
		call realloc (size, buflen, TY_REAL)
	    }

	    Memr[x+n-1] = xval
	    Memr[y+n-1] = yval
	    if (rdmarks)
		Memr[size+n-1] = szmark
	}

	call realloc (x, n, TY_REAL)
	call realloc (y, n, TY_REAL)
	call realloc (size, n, TY_REAL)

	call close (fd)
	call sfree (sp)
	return (n)
end


# GG_LINTRAN -- Linearly transform a vector.  Map pixel values P1,P2
# onto Q1,Q2.

procedure gg_lintran (x, npix, p1in, p2in, q1, q2)

real	x[npix]			# Vector to transform
int	npix			# Number of pixels in vector
real	p1in, p2in		# Range of input values to map
real	q1, q2			# Range for output values
real	p1, p2
real	xscale

begin
	# If P1 and P2 are not set, use full range of input pixels indices.
	if (p1in == 0 && p2in == 0) {
	    p1 = 1.0
	    p2 = npix
	} else {
	    p1 = p1in
	    p2 = p2in
	}

	if (p2 - p1 == 0)
	    xscale = (q2 - q1)
	else
	    xscale = (q2 - q1) / (p2 - p1)

	call asubkr (x, p1, x, npix)
	call amulkr (x, xscale, x, npix)
	call aaddkr (x, q1, x, npix)
end
