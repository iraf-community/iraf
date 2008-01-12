# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<fset.h>
include	<ctype.h>
include	<error.h>
include	<imhdr.h>
include	<pkg/gtools.h>
include	<pkg/xtanswer.h>

define	VERBOSE_OUTPUT	1
define	LIST_OUTPUT	2
define	DEFAULT_OUTPUT	3
define	IMAGE_OP	1
define	LIST_OP		2

define	CF_UNIFORM	1
define	CF_USER		2
define	CF_STATISTICAL	3
define	CF_INSTRUMENTAL	4

define	NADD		20	# Number of points that can be added by ICFIT

# T_CURFIT -- cl interface to the curfit package.  Task CURFIT provides
# four fitting options: legendre, chebyshev, cubic spline or linear spline.
# The output can be printed in default, verbose or tabular formats.  The
# user can also choose to interactively fit the curve.

procedure t_curfit ()

pointer	x, y, w, gt, fcn, fname, flist, dev, str, sp, ic
bool	listdata, verbose, power, redir
int	fd, ofmt, interactive, datatype
int	axis, nvalues, nmax, weighting
pointer	gt_init()
bool	clgetb()
int	imtopen(), clgeti(), cf_operand(), cf_rimage(), cf_rlist()
int	imtgetim(), clgwrd()
int	fstati()

begin
	# Allocate space for string buffers
	call smark (sp)
	call salloc (fcn,   SZ_FNAME, TY_CHAR)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (flist, SZ_LINE,  TY_CHAR)
	call salloc (dev,   SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_FNAME, TY_CHAR)

	# First get cl parameters.  Check to see if input has been redirected.
	redir = false
	if (fstati (STDIN, F_REDIR) == YES) {
	    redir = true
	    call strcpy ("STDIN", Memc[fname], SZ_FNAME)
	} else {
	    call clgstr ("input", Memc[flist], SZ_LINE)
	    fd = imtopen (Memc[flist])
	}

	listdata = clgetb ("listdata")
	verbose  = clgetb ("verbose")
	ofmt = DEFAULT_OUTPUT
	if (listdata)
	    ofmt = LIST_OUTPUT
	else if (verbose)
	    ofmt = VERBOSE_OUTPUT

	# Determine the calculation datatype.
	switch (clgwrd ("calctype", Memc[dev], SZ_FNAME, "|real|double|")) {
	case 1:
	    datatype = TY_REAL
	case 2:
	    datatype = TY_DOUBLE
	}

	if (clgetb ("interactive")) {
	    interactive = YES
	    call clgstr ("device", Memc[dev], SZ_FNAME)
	} else {
	    interactive = ALWAYSNO
	    call strcpy ("", Memc[dev], SZ_FNAME)
	}

	power = clgetb ("power")

	call ic_open (ic)
	call clgstr ("function", Memc[fcn], SZ_FNAME)
	call ic_pstr (ic, "function", Memc[fcn])
	call ic_puti (ic, "order", clgeti ("order"))
	weighting = clgwrd ("weighting", Memc[str], SZ_FNAME,
	    "|uniform|user|statistical|instrumental|")

	gt = gt_init ()

	repeat {
	    if (!redir) {
	        if (imtgetim (fd, Memc[fname], SZ_FNAME) == EOF)
		    break
	    }

	    if (cf_operand (Memc[fname]) == IMAGE_OP) {
		axis = clgeti ("axis")
		nvalues = cf_rimage (Memc[fname], axis, x, y, w, weighting,
		    datatype)
		call gt_sets (gt, GTTYPE, "line")
	    } else {
		nvalues = cf_rlist  (Memc[fname], x, y, w, weighting, datatype)
		call gt_sets (gt, GTTYPE, "mark")

		# For list input only, order the input array.  The
		# rg_ranges package requires an x ordered input array, or else
		# points will be excluded from the fit.  This test can be 
		# removed when/if the ordering restriction is removed from 
		# rg_xranges.  Sorted data is required even when no sampling
		# is done, as in the default case of sample=*.  (ShJ 6-24-88)

		switch (datatype) {
		case TY_REAL:
		    call xt_sort3  (Memr[x], Memr[y], Memr[w], nvalues)
		case TY_DOUBLE:
		    call xt_sort3d (Memd[x], Memd[y], Memd[w], nvalues)
		}
	    }

	    # Allow for adding points.
	    nmax = nvalues + NADD
	    call realloc (x, nmax, datatype)
	    call realloc (y, nmax, datatype)
	    call realloc (w, nmax, datatype)

	    call gt_sets (gt, GTTITLE, Memc[fname])

	    switch (datatype) {
	    case TY_REAL:
	        call cf_fitr (ic, gt, Memr[x], Memr[y], Memr[w], nvalues,
		    nmax, Memc[dev], interactive, ofmt, power)
	    case TY_DOUBLE:
	        call cf_fitd (ic, gt, Memd[x], Memd[y], Memd[w], nvalues,
		    nmax, Memc[dev], interactive, ofmt, power)
	    }

	    call flush (STDOUT)
	    call mfree (x, datatype)
	    call mfree (y, datatype)
	    call mfree (w, datatype)
	     
	    if (redir)
		break
	}

	switch (datatype) {
	case TY_REAL:
	    call ic_closer (ic)
	case TY_DOUBLE:
	    call ic_closed (ic)
	}

	if (!redir)
	    call imtclose (fd)
	call gt_free (gt)
	call sfree (sp)
end


define	IMAGE_OP	1
define	LIST_OP		2

# CF_OPERAND -- Determine whether the operand argument is an image section
# or a list.  If the string is STDIN, it is a list; if a subscript is
# present, it is an image; otherwise we must test whether or not it is a
# binary file and make the decision based on that.

int procedure cf_operand (operand)

char	operand[ARB]		# Input list

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

define	SZ_BUF		1000

# CF_RLIST -- Read a list of two dimensional data pairs into two type
# datatype arrays in memory.  Return pointers to the arrays and a count of the
# number of pixels.  

int procedure cf_rlist (fname, x, y, w, weighting, datatype)

char	fname[ARB]		# Name of list file
pointer	x			# Pointer to x data values (returned)
pointer y			# Pointer to y data values (returned)
pointer	w			# Pointer to weight values (returned)
int	weighting		# Type of weighting
int	datatype		# Datatype of x and Y values

int	buflen, n, fd, ncols, lineno
pointer	sp, lbuf, ip

double	cf_divzd()
int	getline(), nscan(), open()
real	cf_divzr()
extern	cf_divzr(), cf_divzd()
errchk	open, sscan, getline, malloc

begin
	call smark (sp)
	call salloc (lbuf, SZ_LINE, TY_CHAR)

	fd = open (fname, READ_ONLY, TEXT_FILE)

	n = 0
	ncols = 0
	lineno = 0

	while (getline (fd, Memc[lbuf]) != EOF) {
	    # Skip comment lines and blank lines.
	    lineno = lineno + 1
	    if (Memc[lbuf] == '#')
		next
	    for (ip=lbuf;  IS_WHITE(Memc[ip]);  ip=ip+1)
		;
	    if (Memc[ip] == '\n' || Memc[ip] == EOS)
		next

	    if (n == 0) {
		buflen = SZ_BUF
		iferr {
		    call malloc (x, buflen, datatype)
		    call malloc (y, buflen, datatype)
		    call malloc (w, buflen, datatype)
		} then
		    call erract (EA_FATAL)
	    } else if (n + 1 > buflen) {
		buflen = buflen + SZ_BUF
		call realloc (x, buflen, datatype)
		call realloc (y, buflen, datatype)
		call realloc (w, buflen, datatype)
	    }

	    # Decode the points to be plotted.
	    call sscan (Memc[ip])
	    switch (datatype) {
	    case TY_REAL:
		call gargr (Memr[x+n])
		call gargr (Memr[y+n])
		call gargr (Memr[w+n])
	    case TY_DOUBLE:
		call gargd (Memd[x+n])
		call gargd (Memd[y+n])
		call gargd (Memd[w+n])
	    }

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
		if (ncols >= 2) {
		    call eprintf ("only 1 arg; %s, line %d: %s\n")
			call pargstr (fname)
			call pargi (lineno)
			call pargstr (Memc[lbuf])
		    next
		} else {
		    switch (datatype) {
		    case TY_REAL:
			Memr[y+n] = Memr[x+n]
			Memr[x+n] = n + 1.0
			Memr[w+n] = 1.0
		    case TY_DOUBLE:
			Memd[y+n] = Memd[x+n]
			Memd[x+n] = n + 1.0
			Memd[w+n] = 1.0d0
		    }
		}
	    case 2:
		if (ncols == 3) {
		    call eprintf ("only 2 args; %s, line %d: %s\n")
			call pargstr (fname)
			call pargi (lineno)
			call pargstr (Memc[lbuf])
		    next
		} else {
		    switch (datatype) {
		    case TY_REAL:
			Memr[w+n] = 1.0
		    case TY_DOUBLE:
			Memd[w+n] = 1.0d0
		    }
		}
	    }

	    n = n + 1
	}

	call realloc (x, n, datatype)
	call realloc (y, n, datatype)
	call realloc (w, n, datatype)

	switch (weighting) {
	case CF_UNIFORM:
	    if (datatype == TY_REAL)
		call amovkr (1.0, Memr[w], n)
	    else
		call amovkd (1.0d0, Memd[w], n)
	case CF_USER:
	    ;
	case CF_STATISTICAL:
	    if (datatype == TY_REAL) {
		call aabsr (Memr[y], Memr[w], n)
		call arczr (1.0, Memr[w], Memr[w], n, cf_divzr (1.0))
	    } else {
		call aabsd (Memd[y], Memd[w], n)
		call arczd (1.0d0, Memd[w], Memd[w], n, cf_divzd (1.0d0))
	    }
	case CF_INSTRUMENTAL:
	    if (datatype == TY_REAL) {
		call apowkr (Memr[w], 2, Memr[w], n)
		call arczr (1.0, Memr[w], Memr[w], n, cf_divzr (0.0))
	    } else {
		call apowkd (Memd[w], 2, Memd[w], n)
		call arczd (1.0d0, Memd[w], Memd[w], n, cf_divzd (0.0d0))
	    }
	}

	call close (fd)
	call sfree (sp)
	return (n)
end

# CF_RIMAGE -- Read an image section and compute the projection about
# one dimension, producing x and y vectors as output.

int procedure cf_rimage (imsect, axis, x, y, w, weighting, datatype)

char	imsect[ARB]		# Name of image section
pointer	x			# Pointer to x data values
pointer y			# Pointer to y data values
pointer w			# Pointer to weight values
int	weighting		# Type of weighting
int	axis			# Axis about which projection is taken
int	datatype		# Datatype of data values

int	npix
pointer	im
pointer	immap()
errchk	immap, im_projectionr, im_projectiond, malloc

begin
	im = immap (imsect, READ_ONLY, 0)

	if (axis < 1 || axis > IM_NDIM(im))
	    call error (2, "Attempt to take projection over nonexistent axis")
	npix = IM_LEN(im,axis)

	call malloc (x, npix, datatype)
	call malloc (y, npix, datatype)
	call malloc (w, npix, datatype)

	switch (datatype) {
	case TY_REAL:
	    call im_projectionr (im, Memr[x], Memr[y], Memr[w], npix, weighting,
	        axis)
	case TY_DOUBLE:
	    call im_projectiond (im, Memd[x], Memd[y], Memd[w], npix, weighting,
	        axis)
	}

	call imunmap (im)
	return (npix)
end


# CF_DIVZR -- Procedure to return a real number in case of a divide by zero.

real procedure cf_divzr (a)

real	a	# real number

begin
	return (a)
end


# CF_DIVZD -- Procedure to return a double number in case of a divide by zero.

double procedure cf_divzd (a)

double	a	# double precision number number

begin
	return (a)
end
