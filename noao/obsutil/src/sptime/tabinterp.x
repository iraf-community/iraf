include	<error.h>
include	<mach.h>
include	<math/iminterp.h>

# Table structure.
define	TAB_DIM		2			# Maximum dimension of table
define	TAB_SZFNAME	99			# Size of file name
define	TAB_SZPARAMS	(10*SZ_LINE)		# Size of parameter string
define	TAB_SIZE	(55+2*TAB_DIM)		# Size of table structure

define	TAB_FNAME	Memc[P2C($1)]		# File name
define	TAB_VALUE	Memr[P2R($1+50)]	# Constant table value
define	TAB_PARAMS	Memi[$1+51]		# Pointer to parameters
define	TAB_NDIM	Memi[$1+52]		# Dimension of table
define	TAB_INTERP	Memi[$1+53]		# Interpolation pointer
define	TAB_NEXTRAP	Memi[$1+54]		# Number of extrapolations
define	TAB_LEN		Memi[$1+54+$2]		# Length of axes in table
define	TAB_COORD	Memi[$1+54+$2+TAB_DIM]	# Pointer to axes coordinates


procedure tabtest ()

char	table[SZ_FNAME], param[SZ_FNAME], str[SZ_FNAME]
int	clglpr()
real	x, y, z, tabinterp1(), tabinterp2(), tabgetr()
pointer	tab, tabopen()
errchk	tabopen, tabinterp1, tabinterp2, tabgetr, tabgstr

begin
	tab = tabopen ()

	call clgstr ("table", table, SZ_FNAME)

	call clgstr ("param", param, SZ_FNAME)
	z = tabgetr (tab, table, "", param)
	call tabgstr (tab, table, "", param, str, SZ_FNAME)
	call printf ("%s = %g = %s\n")
	    call pargstr (param)
	    call pargr (z)
	    call pargstr (str)

	while (clglpr ("x", x) != EOF) {
	    iferr {
		z = tabinterp1 (tab, table, x)
		call printf ("%g %g\n")
		    call pargr (x)
		    call pargr (z)
	    } then {
		if (clglpr ("y", y) == EOF)
		    break
		z = tabinterp2 (tab, table, x, y)
		call printf ("%g %g %g\n")
		    call pargr (x)
		    call pargr (y)
		    call pargr (z)
	    }
	}

	call tabclose (tab)
end


# TABOPEN -- Open table interpolation package.

pointer procedure tabopen ()

pointer	stp, stopen()

begin
	stp = stopen ("tables", 10, 1000, 1000)
	return (stp)
end


# TABCLOSE -- Close table interpolation package.

procedure tabclose (stp)

pointer	stp		#I Symbol table pointer

pointer	sym, sthead(), stnext()

begin
	for (sym = sthead(stp); sym != NULL; sym = stnext(stp, sym))
	    call tabfree (sym)
	call stclose (stp)
end


procedure tabfree (sym)

pointer	sym		#I Table structure

int	i

begin
	call mfree (TAB_PARAMS(sym), TY_CHAR)
	if (TAB_INTERP(sym) != NULL) {
	    if (TAB_LEN(sym,1) > 1 && TAB_LEN(sym,2) > 1)
		call msifree (TAB_INTERP(sym))
	    else
		call asifree (TAB_INTERP(sym))
	    do i = 1, TAB_NDIM(sym)
		call mfree (TAB_COORD(sym,i), TY_REAL)
	}
end
	


# TABLOAD -- Load a table in the symbol table.

procedure tabload (stp, name, fname)

pointer	stp		# Symbol table pointer
char	name[ARB]	# Name of table
char	fname[ARB]	# File name of table

int	i, fd, ndim, npts, len[2], open(), errget(), nowhite(), ctor(), strlen()
real	value
pointer	sp, str, sym, params, data, coords[2], stfind(), stenter()
bool	streq()
errchk	open, tabread

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# If no value is specified then don't enter into symbol table.
	if (nowhite (fname, Memc[str], SZ_LINE) == 0) {
	    call sfree (sp)
	    return
	}

	# The special string "none" is equivalent to no table.
	if (streq (Memc[str], "none")) {
	    call sfree (sp)
	    return
	}

	# Check if table has already been loaded.
	sym = stfind (stp, name)
	if (sym != NULL) {
	    if (streq (fname, TAB_FNAME(sym)))
		return
	}
	    
	# Check if constant is specified.
	i = 1
	if (ctor (Memc[str], i, value) == strlen (Memc[str])) {
	    sym = stenter (stp, name, TAB_SIZE)
	    call strcpy (fname, TAB_FNAME(sym), TAB_SZFNAME)
	    TAB_VALUE(sym) = value
	    TAB_NDIM(sym) = -1
	    TAB_PARAMS(sym) = NULL
	    TAB_INTERP(sym) = NULL

	    call sfree (sp)
	    return
	}

	# Read the table.
	fd = open (Memc[str], READ_ONLY, TEXT_FILE)
	iferr (call tabread (fd, params, data, npts, len, coords, ndim)) {
	    ndim = errget (Memc[str], SZ_LINE)
	    call strcat (" (", Memc[str], SZ_LINE)
	    call strcat (name, Memc[str], SZ_LINE)
	    call strcat (")", Memc[str], SZ_LINE)
	    call error (1, Memc[str])
	}
	call close (fd)

	if (sym == NULL)
	    sym = stenter (stp, name, TAB_SIZE)
	else
	    call tabfree (sym)

	if (data != NULL) {
	    if (len[1] > 1 && len[2] > 1) {
		call msiinit (TAB_INTERP(sym), II_BILINEAR)
		call msifit (TAB_INTERP(sym), Memr[data], len[1], len[2],
		    len[1])
	    } else if (len[2] == 1) {
		if (len[1] == 1)
		    call asiinit (TAB_INTERP(sym), II_NEAREST)
		else
		    call asiinit (TAB_INTERP(sym), II_LINEAR)
		call asifit (TAB_INTERP(sym), Memr[data], len[1])
	    } else {
		call asiinit (TAB_INTERP(sym), II_LINEAR)
		call asifit (TAB_INTERP(sym), Memr[data], len[2])
	    }
	} else
	    TAB_INTERP(sym) = NULL

	call strcpy (fname, TAB_FNAME(sym), TAB_SZFNAME)
	TAB_PARAMS(sym) = params
	TAB_NDIM(sym) = ndim
	TAB_NEXTRAP(sym) = 0
	call amovi (len, TAB_LEN(sym,1), 2)
	call amovi (coords, TAB_COORD(sym,1), 2)
	call mfree (data, TY_REAL)
	call sfree (sp)
end


# TABREAD -- Read data from a table.

procedure tabread (fd, params, data, npts, len, coords, ndim)

int	fd		#I File descriptor
pointer	params		#O Pointer to parameters
pointer	data		#O Pointer to data 
int	npts		#O Number of data points
int	len[ARB]	#O Number of points along each dimension
pointer	coords[ARB]	#O Pointers to coordinates along each dimension
int	ndim		#O Dimension of table

int	i, j, nread, fdparams, fscan(), nscan(), stropen(), ctor()
real	coord[4], scale
pointer	sp, cmt, key, eq, val
bool	streq()
errchk	stropen()

begin
	iferr {
	    call smark (sp)
	    call salloc (cmt, 2, TY_CHAR)
	    call salloc (key, SZ_FNAME, TY_CHAR)
	    call salloc (eq, 1, TY_CHAR)
	    call salloc (val, SZ_LINE, TY_CHAR)

	    npts = 0
	    ndim = 0
	    params = NULL
	    data = NULL
	    scale = INDEF
	    call aclri (len, 2)
	    call aclri (coords, 2)
	    while (fscan (fd) != EOF) {
		call gargr (coord[1])
		call gargr (coord[2])
		call gargr (coord[3])
		call gargr (coord[4])
		nread = nscan()

		if (nread == 0) {	# Check for parameters.
		    call reset_scan()
		    call gargwrd (Memc[cmt], 2)
		    call gargwrd (Memc[key], SZ_LINE)
		    call gargwrd (Memc[eq], 1)
		    call gargwrd (Memc[val], SZ_LINE)
		    if (nscan() != 4 || Memc[cmt] != '#' || Memc[eq] != '=' ||
			Memc[cmt+1] != EOS)
			    next
		    if (streq (Memc[key], "tablescale")) {
			i = 1
			if (ctor (Memc[val], i, scale) == 0)
			    call error (1, "Syntax error in table scale factor")
		    }
		    if (params == NULL) {
			call malloc (params, TAB_SZPARAMS, TY_CHAR)
			fdparams = stropen (Memc[params], TAB_SZPARAMS,
			    WRITE_ONLY)
		    }
		    call fprintf (fdparams, "%s %s\n")
			call pargstr (Memc[key])
			call pargstr (Memc[val])
		    next
		} else if (nread == 1)
		    next

		if (ndim == 0) {
		    ndim = nread - 1
		    if (ndim > 2)
			call error (1, "Table dimension is too high")
		    do i = ndim+1, 2
			len[i] = 1
		}
		if (nread-1 != ndim)
		    call error (2, "Table has variable number of columns")

		do i = 1, ndim {
		    if (len[i] == 0) {
			call malloc (coords[i], 100, TY_REAL)
			Memr[coords[i]] = coord[i]
			len[i] = len[i] + 1
		    } else {
			do j = 0, len[i] - 1
			    if (coord[i] == Memr[coords[i]+j])
				break
			if (j >= len[i]) {
			    if (mod (len[i], 100) == 0)
				call realloc (coords[i], len[i]+100, TY_REAL)
			    Memr[coords[i]+len[i]] = coord[i]
			    len[i] = len[i] + 1
			}
		    }
		}
		
		if (npts == 0)
		    call malloc (data, 100, TY_REAL)
		else if (mod (npts, 100) == 0)
		    call realloc (data, npts+100, TY_REAL)

		Memr[data+npts] = coord[nread]
		npts = npts + 1
	    }

	    if (npts > 0) {
		j = 1
		do i = 1, ndim
		    j = j * len[i]
		if (j != npts)
		    call error (4, "Table is not regular")
	    }

	    if (!IS_INDEF(scale))
		call amulkr (Memr[data], scale, Memr[data], npts)

	    call close (fdparams)
	    call sfree (sp)
	} then {
	    if (params != NULL) {
		call close (fdparams)
		call mfree (params, TY_CHAR)
	    }
	    do i = 1, 2
		call mfree (coords[i], TY_REAL)
	    call mfree (data, TY_REAL)
	    call sfree (sp)
	    call erract (EA_ERROR)
	}
end


# TABEXISTS -- Determine if table exists.

bool procedure tabexists (stp, name)

pointer	stp		#I Symbol table pointer
char	name[ARB]	#I Name of table

pointer	stfind()

begin
	return (stfind (stp, name) != NULL)
end


# TABGETR -- Get real parameter from table.

real procedure tabgetr (stp, name, alt, param)

pointer	stp		#I Symbol table pointer
char	name[ARB]	#I Name of table
char	alt[ARB]	#I Name of alternate table
char	param[ARB]	#I Parameter name
real	val		#O Returned value

real	rval
int	i, fd, strncmp(), strdic(), stropen(), fscan(), nscan()
bool	streq()
pointer	sp, key, sym[2], stfind()
errchk	stfind, stropen

begin
	# Return if no table.
	if (name[1] == EOS && alt[1] == EOS)
	    return (INDEFR)

	call smark (sp)
	call salloc (key, SZ_FNAME, TY_CHAR)

	# Find tables.
	sym[1] = stfind (stp, name)
	if (alt[1] != EOS)
	    sym[2] = stfind (stp, alt)
	else
	    sym[2] = NULL

	# Return an error if there is no table.
	if (sym[1] == NULL && sym[2] == NULL) {
	    call sprintf (Memc[key], SZ_FNAME, "Table `%s' not found")
		call pargstr (name)
	    call error (1, Memc[key])
	}

	# Get the parameter value.
	val = INDEFR
	do i = 1, 2 {
	    if (sym[i] == NULL)
		next
	    if (strncmp (param, "table.", 6) == 0) {
		switch (strdic (param[7], Memc[key], SZ_FNAME,
		    "|ndim|xmin|xmax|ymin|ymax|nextrap|")) {
		case 1:
		    val = TAB_NDIM(sym[i])
		case 2:
		    if (TAB_NDIM(sym[i]) >= 1)
			val = Memr[TAB_COORD(sym[i],1)]
		    else if (TAB_NDIM(sym[i]) == -1)
			val = -MAX_REAL
		case 3:
		    if (TAB_NDIM(sym[i]) >= 1)
			val = Memr[TAB_COORD(sym[i],1)+TAB_LEN(sym[i],1)-1]
		    else if (TAB_NDIM(sym[i]) == -1)
			val = MAX_REAL
		case 4:
		    if (TAB_NDIM(sym[i]) >= 2)
			val = Memr[TAB_COORD(sym[i],2)]
		    else if (TAB_NDIM(sym[i]) == -1)
			val = -MAX_REAL
		case 5:
		    if (TAB_NDIM(sym[i]) >= 2)
			val = Memr[TAB_COORD(sym[i],2)+TAB_LEN(sym[i],1)-1]
		    else if (TAB_NDIM(sym[i]) == -1)
			val = MAX_REAL
		case 6:
		    val = TAB_NEXTRAP(sym[i])
		}
		break

	    } else if (TAB_PARAMS(sym[i]) != NULL) {
		fd = stropen (Memc[TAB_PARAMS(sym[i])], TAB_SZPARAMS, READ_ONLY)
		while (fscan (fd) != EOF) {
		    call gargwrd (Memc[key], SZ_FNAME)
		    call gargr (rval)
		    if (nscan() != 2)
			next
		    if (streq (Memc[key], param)) {
			val = rval
			break
		    }
		}
		call close (fd)
	    }

	    if (!IS_INDEF(val))
		break
	}

	# Return error if no value was found.
	if (IS_INDEF(val)) {
	    call sprintf (Memc[key], SZ_FNAME,
		"Table parameter `%s' not found (%s)")
		call pargstr (param)
		call pargstr (name)
	    call error (1, Memc[key])
	}

	call sfree (sp)
	return (val)

end


# TABGETI -- Get integer paraemter from table.

int procedure tabgeti (stp, name, alt, param)

pointer	stp		#I Symbol table pointer
char	name[ARB]	#I Name of table
char	alt[ARB]	#I Name of alternate table
char	param[ARB]	#I Parameter name

real	val, tabgetr()
errchk	tabgetr

begin
	val = tabgetr (stp, name, alt, param)
	if (IS_INDEFR(val))
	    return (INDEFI)

	return (nint(val))
end


# TABGSTR -- Get string parameter from table.

procedure tabgstr (stp, name, alt, param, val, maxchar)

pointer	stp		#I Symbol table pointer
char	name[ARB]	#I Name of table
char	alt[ARB]	#I Name of alternate table
char	param[ARB]	#I Parameter name
char	val[ARB]	#O Returned value
int	maxchar		#I Maximum string length

int	i, fd, strncmp(), strdic(), stropen(), fscan(), nscan()
bool	streq()
pointer	sp, key, str, sym[2], stfind()
errchk	stfind, stropen()

begin
	# Return if no table.
	if (name[1] == EOS && alt[1] == EOS) {
	    val[1] = EOS
	    return
	}

	call smark (sp)
	call salloc (key, SZ_FNAME, TY_CHAR)
	call salloc (str, maxchar, TY_CHAR)

	# Find tables.
	sym[1] = stfind (stp, name)
	if (alt[1] != EOS)
	    sym[2] = stfind (stp, alt)
	else
	    sym[2] = NULL

	# Return an error if there is no table.
	if (sym[1] == NULL && sym[2] == NULL) {
	    call sprintf (Memc[key], SZ_FNAME, "Table `%s' not found")
		call pargstr (name)
	    call error (1, Memc[key])
	}

	# Get the parameter value.
	val[1] = EOS
	do i = 1, 2 {
	    if (sym[i] == NULL)
		next
	    if (strncmp (param, "table.", 6) == 0) {
		switch (strdic (param[7], Memc[key], SZ_FNAME, "|filename|")) {
		case 1:
		    call strcpy (TAB_FNAME(sym[i]), val, maxchar)
		}
		break

	    } else if (TAB_PARAMS(sym[i]) != NULL) {
		fd = stropen (Memc[TAB_PARAMS(sym[i])], TAB_SZPARAMS, READ_ONLY)
		while (fscan (fd) != EOF) {
		    call gargwrd (Memc[key], SZ_FNAME)
		    call gargstr (Memc[str], SZ_LINE)
		    if (nscan() != 2)
			next
		    if (streq (Memc[key], param)) {
			if (val[1] == EOS)
			    call strcpy (Memc[str+1], val, maxchar)
			else {
			    call strcat ("\n", val, maxchar)
			    call strcat (Memc[str+1], val, maxchar)
			}
		    }
		}
		call close (fd)

		if (val[1] != EOS)
		    break
	    }
	}

	# Return error if no value was found.
	if (val[1] == EOS) {
	    call sprintf (Memc[key], SZ_FNAME,
		"Table parameter `%s' not found (%s)")
		call pargstr (param)
		call pargstr (name)
	    call error (1, Memc[key])
	}

	call sfree (sp)
end


# TABINTERP1 -- Interpolate a named 1D table.

real procedure tabinterp1 (stp, name, x)

pointer	stp		# Symbol table pointer
char	name[ARB]	# Name of table
real	x		# Interpolation coordinate

char	err[SZ_FNAME]
int	i, nx
real	xi, y, asieval()
pointer	sym, xp, stfind()
errchk	stfind

begin
	# Find the table.
	sym = stfind (stp, name)
	if (sym == NULL) {
	    call sprintf (err, SZ_FNAME, "Table `%s' not found")
		call pargstr (name)
	    call error (1, err)
	}

	# If a constant table return the value.
	if (TAB_NDIM(sym) == -1)
	    return (TAB_VALUE(sym))

	# Check if the table is of the proper dimensionality.
	if (TAB_NDIM(sym) != 1) {
	    call sprintf (err, SZ_FNAME, "Table is not one dimensional (%s)")
	       call pargstr (name)
	    call error (1, err)
	}
	
	nx = TAB_LEN(sym,1)
	xp = TAB_COORD(sym,1)
	if (x < Memr[xp] || x > Memr[xp+nx-1]) 
	    TAB_NEXTRAP(sym) = TAB_NEXTRAP(sym) + 1

	if (nx == 1)
	    xi = 1
	else {
	    do i = 1, nx-2
		if (x < Memr[xp+i])
		    break

	    xi = i + (x - Memr[xp+i-1]) / (Memr[xp+i] - Memr[xp+i-1])
	}
	xi = max (1., min (real(nx), xi))
	y = asieval (TAB_INTERP(sym), xi)


	return (y)
end


# TABINTERP2 -- Interpolate a named 2D table.

real procedure tabinterp2 (stp, name, x, y)

pointer	stp		# Symbol table pointer
char	name[ARB]	# Name of table
real	x, y		# Interpolation coordinate

char	err[SZ_FNAME]
int	i, nx, ny
real	xi, yi, z, asieval(), msieval()
pointer	sym, xp, yp, stfind()
errchk	stfind

begin
	# Find the table.
	sym = stfind (stp, name)
	if (sym == NULL) {
	    call sprintf (err, SZ_FNAME, "Table `%s' not found")
		call pargstr (name)
	    call error (1, err)
	}

	# If a constant table return the value.
	if (TAB_NDIM(sym) == -1)
	    return (TAB_VALUE(sym))

	# Check if the table is of the proper dimensionality.
	if (TAB_NDIM(sym) != 2) {
	    call sprintf (err, SZ_FNAME, "Table is not two dimensional (%s)")
	       call pargstr (name)
	    call error (1, err)
	}
	
	nx = TAB_LEN(sym,1)
	ny = TAB_LEN(sym,2)
	if (nx > 1 && ny > 1) {		# 2D interpolation
	    xp = TAB_COORD(sym,1)
	    do i = 1, nx-2
		if (x < Memr[xp+i])
		    break
	    xi = i + (x - Memr[xp+i-1]) / (Memr[xp+i] - Memr[xp+i-1])
	    xi = max (1., min (real(nx), xi))
	    yp = TAB_COORD(sym,2)
	    do i = 1, ny-2
		if (y < Memr[yp+i])
		    break
	    yi = i + (y - Memr[yp+i-1]) / (Memr[yp+i] - Memr[yp+i-1])
	    yi = max (1., min (real(nx), yi))
	    z = msieval (TAB_INTERP(sym), xi, yi)
	} else if (ny == 1) {		# 1D interpolation in x
	    if (nx == 1)
		xi = 1
	    else {
		xp = TAB_COORD(sym,1)
		do i = 1, nx-2
		    if (x < Memr[xp+i])
			break

		xi = i + (x - Memr[xp+i-1]) / (Memr[xp+i] - Memr[xp+i-1])
	    }
	    xi = max (1., min (real(nx), xi))
	    z = asieval (TAB_INTERP(sym), xi)
	} else {			# 1D interpolation in y
	    yp = TAB_COORD(sym,2)
	    do i = 1, ny-2
		if (y < Memr[yp+i])
		    break

	    yi = i + (y - Memr[yp+i-1]) / (Memr[yp+i] - Memr[yp+i-1])
	    yi = max (1., min (real(ny), yi))
	    z = asieval (TAB_INTERP(sym), yi)
	}

	return (z)
end
