# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<xwhen.h>
include	<config.h>
include	<imhdr.h>
include	<mach.h>
include	<error.h>
include	<ctype.h>
include	<fset.h>
include	<gset.h>

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

char	device[SZ_FNAME]
int	mode, i, window
int	tgrjmp[LEN_JUMPBUF], epa, old_onint, status

bool	clgetb()
int	fstati()
extern	tgr_onint()
data	window /0/
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
	mode = NEW_FILE
	if (clgetb ("append"))
	    mode = APPEND

	# Install interrupt exception handler.
	call zlocpr (tgr_onint, epa)
	call xwhen (X_INT, epa, old_onint)

	call zsvjmp (tgrjmp, status)
	if (status == OK) {
	    # Fetch remaining params and draw the plot.
	    iferr (call ggplot (device, mode, input, x, y, size, npix, ncurves))
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

procedure ggplot (device, mode, input, x, y, size, npix, ncurves)

char	device[SZ_FNAME]	# Graphics device
int	mode			# Mode of graphics stream
char	input[ARB]		# List of operands to be plotted
pointer	x[MAX_CURVES]		# X values
pointer y[MAX_CURVES]		# Y values
pointer size[MAX_CURVES]	# Size of markers to plot
int	npix[MAX_CURVES]	# Number of points per curve
int	ncurves			# Number of curves to overplot

pointer	gd
char	xlabel[SZ_LINE], ylabel[SZ_LINE], title[SZ_LINE]
char	marker[SZ_FNAME]
bool	pointmode, lintran, xautoscale, yautoscale
bool	drawbox, transpose, rdmarks
int	xtran, ytran, axis, ticklabels, i, marker_type, j
real	p1, p2, q1, q2, wx1, wx2, wy1, wy2, szmarker, vx1, vx2, vy1, vy2
real	xx, yy, sz, szx, szy
pointer	ptemp

pointer	gopen()
bool	clgetb(), streq(), fp_equalr()
int	clgeti(), gg_rdcurves()
real	clgetr()
errchk	clgetb, clgeti, clgstr, clgetr, glabax, gpmark
errchk	gg_setdashpat, gswind, gseti, gg_rdcurves, gascale, grscale

begin
	# If computing projection along an axis (collapsing a multidimensional
	# section to a vector), fetch axis number.
	axis = clgeti ("axis")

	# Initialize dashline index
	call gg_initdashpat ()

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
	}

	# Read all the curves specified by the operands in input into memory.
	ncurves = gg_rdcurves (input, x, y, size, npix, axis, rdmarks)

	gd = gopen (device, mode, STDGRAPH)

	xautoscale = false
	yautoscale = false

	# Set window and viewport.  If user window has not been set, enable
	# autoscaling.  If device viewport has not been set, let glabax
	# handle the viewport internally.

	if (mode != APPEND) {
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
	if (mode != APPEND)
	    if (clgetb ("box"))
		drawbox = true

	if (drawbox) {
	    # Get number of major and minor tick marks.
	    call gseti (gd, G_XNMAJOR, clgeti ("majrx"))
	    call gseti (gd, G_XNMINOR, clgeti ("minrx"))
	    call gseti (gd, G_YNMAJOR, clgeti ("majry"))
	    call gseti (gd, G_YNMINOR, clgeti ("minry"))

	    # Fetch labels and plot title string. 

	    call clgstr ("xlabel", xlabel, SZ_LINE)
	    call clgstr ("ylabel", ylabel, SZ_LINE)

	    call clgstr ("title", title, SZ_LINE)
	    if (streq (title, "imtitle"))
		call gg_get_imtitle (input, title, SZ_LINE)

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

	if (mode == APPEND) {
	    call ggeti (gd, G_XTRAN, xtran)
	    call ggeti (gd, G_YTRAN, ytran)
	    call ggwind (gd, wx1, wx2, wy1, wy2)
	} else {
	    xtran = GW_LINEAR
	    if (clgetb ("logx"))
		xtran = GW_LOG
	    ytran = GW_LINEAR
	    if (clgetb ("logy"))
		ytran = GW_LOG
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
	    call gseti (gd, G_LABELTICKS, ticklabels)
	    call glabax (gd, title, xlabel, ylabel)
	}

	# Draw the curves.
	do i = 1, ncurves {
	    if (pointmode) {
		if (!rdmarks) {
		    call amovkr (szmarker, Memr[size[i]], npix[i])
		    call gpmark (gd, Memr[x[i]], Memr[y[i]], npix[i], 
		        marker_type, Memr[size[i]], Memr[size[i]])
		} else {
		    if (szmarker < 0)
		        call amulkr (Memr[size[i]], -szmarker, Memr[size[i]],
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
	    } else {
		call gg_setdashpat (gd)
		call gpline (gd, Memr[x[i]], Memr[y[i]], npix[i])
	    }
	}

	call gclose (gd)
end


# GG_RDCURVES -- Given the operand list as input, read in all the referenced
# lists and/or image sections, producing a list of vectors as output.  Return
# as the function value the number of curves.

int procedure gg_rdcurves (oplist, x, y, size, npix, axis, rdmarks)

char	oplist[ARB]		# Operand list
pointer	x[ARB]			# Pointer to x vector
pointer	y[ARB]			# Pointer to y vector
pointer	size[ARB]		# Pointer to vector of marker sizes
int	npix[ARB]		# Number of values per vector
int	axis			# Axis for projection
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
		npix[i] = gg_rdcurve (operand, x[i],y[i],size[i], axis,rdmarks)
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
# input routine to access the data.

int procedure gg_rdcurve (operand, x, y, size, axis, rdmarks)

char	operand[ARB]		# List of operaands to be plotted
pointer	x, y, size		# Pointers to x, y and size arrays
int	axis			# Axis of image projection
bool	rdmarks			# Read marks from list?

int	gg_rdlist2(), gg_rdimage2(), gg_optype()
errchk	gg_rdlist2, gg_rdimage2, gg_optype

begin
	if (gg_optype (operand) == LIST_OP)
	    return (gg_rdlist2 (operand, x, y, size, rdmarks))
	else
	    return (gg_rdimage2 (operand, x, y, size, axis))
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
# one dimension, producing x and y vectors as output.

int procedure gg_rdimage2 (imsect, x, y, size, axis)

char	imsect[ARB]		# Image section to be plotted
pointer	x, y, size		# Pointer to x, y and size vector
int	axis			# Axis about which the projection is to be taken

int	npix, i
pointer	im
pointer	immap()
errchk	immap, im_projection, malloc

begin
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

	do i = 1, npix
	    Memr[x+i-1] = i

	call imunmap (im)
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


# GG_GET_IMTITLE -- Given the operand list as input, fetch the first operand
# and try to open it as an image.  If the operand is an image return the
# title string as an output argument, otherwise return the null string.
# If there are multiple operands only the first is used.

procedure gg_get_imtitle (oplist, title, maxch)

char	oplist[ARB]		# List of operands to be graphed
char	title[ARB]		# Image title string (output)
int	maxch

char	operand[SZ_FNAME], section[SZ_FNAME]
int	fd
pointer	im
int	imtopen(), imtgetim(), gg_optype(), stridxs(), imtlen()
pointer	immap()
errchk	imtopen(), imtgetim()

begin
	title[1] = EOS

	# Get the first operand name.  If it is not an image we are all done.
	fd = imtopen (oplist)

	if (imtgetim (fd, operand, SZ_FNAME) == EOF)
	    return
	if (gg_optype (operand) != IMAGE_OP)
	    return

	# Open the image.
	iferr (im = immap (operand, READ_ONLY, 0))
	    return

	# Get image title.
	call strcpy (IM_TITLE(im), title, maxch)
	if (stridxs ("\n", title) == 0)
	    call strcat ("\n", title, maxch)

	# If an image section was given and there was only one operand in
	# the operand list, append the section to the title.

	call imgsection (operand, section, SZ_FNAME)

	if (section[1] != EOS) {
	    if (imtlen(fd) == 1)
		call strcat (section, title, maxch)
	}

	call imunmap (im)
	call imtclose (fd)
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


# GG_INITDASHPAT -- Initialize index of dash pattern array

procedure gg_initdashpat ()

int	ip
common  /dashinit/ip

begin
	ip = 0
end


# GG_SETDASHPAT -- Set the current dash pattern to the next one in the 
# available list.

procedure gg_setdashpat (gd)

pointer	gd			# Pointer to graphics stream

int	patterns[4]
int	ip
common	/dashinit/ip
data	patterns/GL_SOLID, GL_DASHED, GL_DOTTED, GL_DOTDASH/

begin
	if (ip < 4)
	    ip = ip + 1
	call gseti (gd, G_PLTYPE, patterns[ip])
end
