include	<error.h>
include	<evvexpr.h>
include	<ctype.h>
include	<chars.h>
include	<mach.h>
include	<lexnum.h>

define	SZ_FIELD	8
define	SZ_EXPR		160


# T_ASTHEDIT -- Edit/calculate fields in an image header or headers.  This
# task performs a series of operations defined in a file.  It has various
# astronomical utility functions.

procedure t_asthedit()

int	imlist			# list of images
pointer	cmds			# file of commands
pointer	table			# data table
pointer	colnames		# column names
bool	verbose			# verbose output

int	i, fd, ncmds, ncols, nlines
pointer	sp, image, field, expr, fields, exprs, colvals, oldval, newval, ptr
pointer	im, immap()
int	imtopenp(), imtgetim(), imtlen()
int	open(), clscan(), fscan(), nscan(), nowhite()
bool	clgetb()
errchk	open

common	/ahgopm/ im, colnames, colvals, ncols

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (cmds, SZ_FNAME, TY_CHAR)
	call salloc (table, SZ_FNAME, TY_CHAR)
	call salloc (field, SZ_FIELD, TY_CHAR)
	call salloc (expr, SZ_EXPR,  TY_CHAR)
	call salloc (oldval, SZ_LINE,  TY_CHAR)
	call salloc (newval, SZ_LINE,  TY_CHAR)

	# Get parameters.
	imlist = imtopenp ("images")
	call clgstr ("commands", Memc[cmds], SZ_FNAME)
	verbose = clgetb ("verbose")

	# Read command file.
	ncmds = 0
	fd = open (Memc[cmds], READ_ONLY, TEXT_FILE)
	while (fscan (fd) != EOF) {
	    call gargwrd (Memc[field], SZ_FIELD)
	    call gargstr (Memc[expr], SZ_EXPR)
	    if (nscan() < 2)
		next
	    if (ncmds == 0) {
		call malloc (fields, 100, TY_POINTER)
		call malloc (exprs, 100, TY_POINTER)
	    } else if (mod (ncmds, 100) == 0) {
		call realloc (fields, ncmds+100, TY_POINTER)
		call realloc (exprs, ncmds+100, TY_POINTER)
	    }

	    call salloc (Memi[fields+ncmds], SZ_FIELD, TY_CHAR)
	    call salloc (Memi[exprs+ncmds], SZ_EXPR, TY_CHAR)
	    call strcpy (Memc[field], Memc[Memi[fields+ncmds]], SZ_FIELD)
	    for (ptr=expr; IS_WHITE(Memc[ptr]); ptr=ptr+1)
		;
	    if (Memc[ptr] == EOS)
		Memc[Memi[exprs+ncmds]] = DLE
	    else
		call strcpy (Memc[ptr], Memc[Memi[exprs+ncmds]], SZ_EXPR)
	    ncmds = ncmds + 1
	}
	call close (fd)
	if (ncmds == 0)
	    call error (1, "No commands in command file")
	call salloc (ptr, ncmds, TY_POINTER)
	call amovi (Memi[fields], Memi[ptr], ncmds)
	call mfree (fields, TY_POINTER)
	fields = ptr
	call salloc (ptr, ncmds, TY_POINTER)
	call amovi (Memi[exprs], Memi[ptr], ncmds)
	call mfree (exprs, TY_POINTER)
	exprs = ptr

	# Get the column names, open the table file, and check the file.
	ncols = 0
	if (clscan ("colnames") == EOF)
	    call error (1, "Error reading column names")
	repeat {
	    call gargwrd (Memc[field], SZ_FIELD)
	    if (nscan() == ncols)
		break
	    if (ncols == 0)
		call malloc (colnames, 10, TY_POINTER)
	    else if (mod (ncols, 10) == 0)
		call realloc (colnames, ncols+10, TY_POINTER)
	    call salloc (Memi[colnames+ncols], SZ_FIELD, TY_CHAR)
	    call strcpy (Memc[field], Memc[Memi[colnames+ncols]], SZ_FIELD)
	    ncols = ncols + 1
	}

	fd = NULL
	if (ncols > 0) {
	    call salloc (ptr, ncols, TY_POINTER)
	    call amovi (Memi[colnames], Memi[ptr], ncols)
	    call mfree (colnames, TY_POINTER)
	    colnames = ptr

	    call clgstr ("table", Memc[table], SZ_FNAME)
	    if (nowhite (Memc[table], Memc[table], SZ_FNAME) > 0) {
		fd = open (Memc[table], READ_ONLY, TEXT_FILE)
		call salloc (colvals, ncols, TY_POINTER)
		do i = 1, ncols
		    call salloc (Memi[colvals+i-1], SZ_LINE, TY_CHAR)
		nlines = 0
		while (fscan (fd) != EOF) {
		    do i = 1, ncols
			call gargwrd (Memc[Memi[colvals+i-1]], SZ_LINE)
		    if (nscan() == ncols)
			nlines = nlines + 1
		}
		if (nlines != imtlen (imlist)) {
		    call close (fd)
		    call sfree (sp)
		    call error (1,
		       "Table file has incorrect number of data lines")
		}
		call seek (fd, BOF)
	    }
	}

	# Loop through the images.
	while (imtgetim (imlist, Memc[image], SZ_FNAME) != EOF) {
	    iferr (im = immap (Memc[image], READ_WRITE, 0)) {
		call erract (EA_WARN)
		next
	    }
	    if (fd != NULL) {
		repeat {
		    i = fscan (fd)
		    do i = 1, ncols
			call gargwrd (Memc[Memi[colvals+i-1]], SZ_LINE)
		    if (nscan() == ncols)
			break
		}
	    }

	    if (verbose) {
		call printf ("%s:\n")
		    call pargstr (Memc[image])
	    }

	    do i = 1, ncmds {
		field = Memi[fields+i-1]
		expr = Memi[exprs+i-1]
		call ah_evaluate (im, Memc[field], Memc[expr], Memc[oldval],
		    Memc[newval], verbose)
	    }

	    call imunmap (im)
	}

	if (fd != NULL)
	    call close (fd)
	call imtclose (imlist)
	call sfree (sp)
end


# AH_EVALUATE -- Evaluate the value of the named field and add it.

procedure ah_evaluate (im, field, expr, oldval, newval, verbose)

pointer	im			#I image pointer
char	field[ARB]		#I name of field to be edited
char	expr[ARB]		#I value expression
char	oldval[SZ_LINE]		#O old value
char	newval[SZ_LINE]		#O new value
bool	verbose			#I verbose output?

bool	b
pointer	o, evvexpr()
int	locpr()
extern	ah_getop(), ah_fcn()
errchk	evvexpr

begin
	# Evaluate the expression.
	o = NULL
	if (expr[1] != DLE)
	    o = evvexpr (expr, locpr (ah_getop), NULL, locpr (ah_fcn), NULL, 0)

	# Print the verbose output.
	if (verbose) {
	    if (o == NULL)
		call sprintf (newval, SZ_LINE, "DELETED")
	    else {
		switch (O_TYPE(o)) {
		case TY_BOOL:
		    call sprintf (newval, SZ_LINE, "%b")
			call pargi (O_VALI(o))
		case TY_CHAR:
		    call sprintf (newval, SZ_LINE, "%s")
			call pargstr (O_VALC(o))
		case TY_INT:
		    call sprintf (newval, SZ_LINE, "%d")
			call pargi (O_VALI(o))
		case TY_REAL:
		    call sprintf (newval, SZ_LINE, "%g")
			call pargr (O_VALR(o))
		case TY_DOUBLE:
		    call sprintf (newval, SZ_LINE, "%g")
			call pargd (O_VALD(o))
		}
	    }

	    iferr (call imgstr (im, field, oldval, SZ_LINE)) {
		call printf ("  %s = %s\n")
		    call pargstr (field)
		    call pargstr (newval)
	    } else {
		call printf ("  %s = %s -> %s\n")
		    call pargstr (field)
		    call pargstr (oldval)
		    call pargstr (newval)
	    }
	}

	# Store the result.
	iferr (call imdelf (im, field))
	    ;
	if (o != NULL) {
	    switch (O_TYPE(o)) {
	    case TY_BOOL:
		b = (O_VALI(o) == YES)
		call imaddb (im, field, b)
	    case TY_CHAR:
		call imastr (im, field, O_VALC(o))
	    case TY_INT:
		call imaddi (im, field, O_VALI(o))
	    case TY_REAL:
		call imaddr (im, field, O_VALR(o))
	    case TY_DOUBLE:
		call imaddd (im, field, O_VALD(o))
	    }

	    call mfree (o, TY_STRUCT)
	}
end


# AH_GETOP -- Satisfy an operand request from EVEXPR.  In this context,
# operand names refer to the fields of the image header.  If there
# is no value in the image header query.

procedure ah_getop (data, operand, o)

pointer	data			#I Client data
char	operand[ARB]		#I name of operand to be returned
pointer	o			#O pointer to output operand

int	ip, type, nchars
pointer	val
bool	streq()
int	imaccf(), imgeti(), imgftype(), lexnum(), ctoi(), ctod()
double	imgetd()

pointer	im, colnames, colvals
int	ncols
common	/ahgopm/ im, colnames, colvals, ncols

begin
	# Get operand from table or image header.
	if (operand[1] == '$') {
	    do ip = 1, ncols
		if (streq (operand[2], Memc[Memi[colnames+ip-1]]))
		    break
	    if (ip > ncols)
		call xvv_error1 ("table column `%s' not found", operand[2])

	    val = Memi[colvals+ip-1]
	    ip = 1
	    type = lexnum (Memc[val], ip, nchars)
	    if (Memc[val+nchars+ip-1] != EOS)
		type = LEX_NONNUM

	    switch (type) {
	    case LEX_OCTAL, LEX_DECIMAL, LEX_HEX:
		call xvv_initop (o, 0, TY_INT)
		ip = 1
		nchars = ctoi (Memc[val], ip, O_VALI(o))
	    case LEX_REAL:
		call xvv_initop (o, 0, TY_DOUBLE)
		ip = 1
		nchars = ctod (Memc[val], ip, O_VALD(o))
	    case LEX_NONNUM:
		call xvv_initop (o, SZ_LINE, TY_CHAR)
		call strcpy (Memc[val], O_VALC(o), SZ_LINE)
	    }

	    return

	} else {
	    if (imaccf (im, operand) == NO)
		call xvv_error1 ("image keyword `%s' not found", operand[2])
	    switch (imgftype (im, operand)) {
	    case TY_BOOL, TY_SHORT, TY_INT, TY_LONG:
		call xvv_initop (o, 0, TY_INT)
		O_VALI(o) = imgeti (im, operand)

	    case TY_REAL, TY_DOUBLE, TY_COMPLEX:
		call xvv_initop (o, 0, TY_DOUBLE)
		O_VALD(o) = imgetd (im, operand)

	    default:
		call malloc (val, SZ_LINE, TY_CHAR)
		call imgstr (im, operand, Memc[val], SZ_LINE)

		ip = 1
		type = lexnum (Memc[val], ip, nchars)
		if (Memc[val+nchars+ip-1] != EOS)
		    type = LEX_NONNUM

		switch (type) {
		case LEX_OCTAL, LEX_DECIMAL, LEX_HEX:
		    call xvv_initop (o, 0, TY_INT)
		    ip = 1
		    nchars = ctoi (Memc[val], ip, O_VALI(o))
		case LEX_REAL:
		    call xvv_initop (o, 0, TY_DOUBLE)
		    ip = 1
		    nchars = ctod (Memc[val], ip, O_VALD(o))
		case LEX_NONNUM:
		    call xvv_initop (o, SZ_LINE, TY_CHAR)
		    call strcpy (Memc[val], O_VALC(o), SZ_LINE)
		}

		call mfree (val, TY_CHAR)
	    }
	}
end


# Special functions
define	KEYWORDS "|sexstr|epoch|julday|mst|ra_precess|dec_precess|airmass|\			  |eairmass|obsdb|"

define	F_SEXSTR		1	# sexstr (value)
define	F_EPOCH			2	# epoch (date, ut)
define	F_JULDAY		3	# julday (date, ut)
define	F_MST			4	# mst (date, ut, longitude)
define	F_RAPRECESS		5	# ra_precess (ra, dec, epoch1, epoch2)
define	F_DECPRECESS		6	# dec_precess (ra, dec, epoch1, epoch2)
define	F_AIRMASS		7	# airmass (ra, dec, st, latitude)
define	F_EAIRMASS		9	# eairmass (ra, dec, st, exptime, lat)
define	F_OBSDB			10	# obsdb (observatory, parameter)

define  SOLTOSID        (($1)*1.00273790935d0)

# AH_FCN -- Special functions called by expression parser.

procedure ah_fcn (data, fcn, args, nargs, out)

pointer	data			#I client data
char	fcn[ARB]		#I function to be called
pointer	args[ARB]		#I pointer to arglist descriptor
int	nargs			#I number of arguments
pointer	out			#O output operand (function value)

int	yr, mo, day
double	time, epoch, ra, dec, longitude, latitude
double	ast_julday(), ast_mst(), airmass()

double	dresult
int	iresult, optype, oplen
int	opcode, v_nargs, i, ip
pointer	sp, buf, dval, obs

bool	strne()
pointer	obsopen()
int	strdic(), ctoi(), ctod(), btoi()
errchk	zcall4, xvv_error1, xvv_error2, malloc, obsopen, obsgstr
define	badtype_ 91
define	free_ 92

begin
	call smark (sp)
	call salloc (buf, SZ_FNAME, TY_CHAR)
	call salloc (dval, nargs, TY_DOUBLE)

	oplen = 0

	# Lookup the function name in the dictionary.  An exact match is
	# required (strdic permits abbreviations).

	opcode = strdic (fcn, Memc[buf], SZ_FNAME, KEYWORDS)
	if (opcode > 0 && strne(fcn,Memc[buf]))
	    opcode = 0

	# Abort if the function is not known.

	if (opcode <= 0)
	    call xvv_error1 ("unknown function `%s' called", fcn)

	# Verify correct number of arguments.
	switch (opcode) {
	case F_SEXSTR:
	    v_nargs = -1
	case F_EPOCH, F_JULDAY:
	    v_nargs = 2
	case F_MST:
	    v_nargs = 3
	case F_RAPRECESS, F_DECPRECESS:
	    v_nargs = 4
	case F_AIRMASS:
	    v_nargs = 4
	case F_EAIRMASS:
	    v_nargs = 5
	case F_OBSDB:
	    v_nargs = 2
	default:
	    v_nargs = 1
	}

	if (v_nargs > 0 && nargs != v_nargs)
	    call xvv_error2 ("function `%s' requires %d arguments",
		fcn, v_nargs)
	else if (v_nargs < 0 && nargs < abs(v_nargs))
	    call xvv_error2 ("function `%s' requires at least %d arguments",
		fcn, abs(v_nargs))

	# Convert datatypes to double.
	do i = 1, nargs {
	    switch (O_TYPE(args[i])) {
	    case TY_CHAR:
		ip = 1
		if (ctod (O_VALC(args[i]), ip, Memd[dval+i-1]) == 0)
		    Memd[dval+i-1] = 0.
	    case TY_INT:
		Memd[dval+i-1] = O_VALI(args[i])
	    case TY_REAL:
		Memd[dval+i-1] = O_VALR(args[i])
	    case TY_DOUBLE:
		Memd[dval+i-1] = O_VALD(args[i])
	    }
	}


	# Expand date and time.
	switch (opcode) {
	case F_EPOCH, F_JULDAY, F_MST:
	    ip = 1
	    if (ctoi (O_VALC(args[1]), ip, day) == 0)
		call xvv_error ("unrecognized date format")
	    ip = ip + 1
	    if (ctoi (O_VALC(args[1]), ip, mo) == 0)
		call xvv_error ("unrecognized date format")
	    ip = ip + 1
	    if (ctoi (O_VALC(args[1]), ip, yr) == 0)
		call xvv_error ("unrecognized date format")
	    time = Memd[dval+1]
	    call ast_date_to_epoch (yr, mo, day, time, epoch)
	}

	# Evaluate the function.

	optype = TY_DOUBLE
	switch (opcode) {
	case F_SEXSTR:
	    optype = TY_CHAR
	    oplen = MAX_DIGITS
	    call malloc (iresult, oplen, TY_CHAR)
	    call sprintf (Memc[iresult], SZ_FNAME, "%.*h")
		if (nargs > 1)
		    call pargi (max (0, nint (Memd[dval+1])))
		else
		    call pargi (0)
		call pargd (Memd[dval]+1E-7)
	    
	case F_EPOCH:
	    dresult = epoch

	case F_JULDAY:
	    dresult = ast_julday (epoch)

	case F_MST:
	    longitude = Memd[dval+2]
	    dresult = ast_mst (epoch, longitude)

	case F_RAPRECESS:
	    call ast_precess (Memd[dval], Memd[dval+1], Memd[dval+2],
		ra, dec, Memd[dval+3])
	    dresult = ra

	case F_DECPRECESS:
	    call ast_precess (Memd[dval], Memd[dval+1], Memd[dval+2],
		ra, dec, Memd[dval+3])
	    dresult = dec

	case F_AIRMASS:
	    ra = Memd[dval]
	    dec = Memd[dval+1]
	    time = Memd[dval+2]
	    latitude = Memd[dval+3]
	    dresult = airmass (time-ra, dec, latitude)

	case F_EAIRMASS:
	    ra = Memd[dval]
	    dec = Memd[dval+1]
	    time = Memd[dval+2]
	    Memd[dval+3] = Memd[dval+3] / 3600.
	    latitude = Memd[dval+4]
	    dresult = airmass (time-ra, dec, latitude)
	    time = time + SOLTOSID(Memd[dval+3]) / 2.
	    dresult = dresult + 4 * airmass (time-ra, dec, latitude)
	    time = time + SOLTOSID(Memd[dval+3]) / 2.
	    dresult = dresult + airmass (time-ra, dec, latitude)
	    dresult = dresult / 6.

	case F_OBSDB:
	    optype = TY_CHAR
	    oplen = SZ_LINE
	    call malloc (iresult, oplen, TY_CHAR)
	    obs = obsopen (O_VALC(args[1]))
	    call obsgstr (obs, O_VALC(args[2]), Memc[iresult], oplen)
	    call obsclose (obs)

	default:
	    call xvv_error ("bad switch in userfcn")
	}

	# Format sexigesimal strings.
	switch (opcode) {
	case F_MST, F_RAPRECESS, F_DECPRECESS:
	    optype = TY_CHAR
	    oplen = MAX_DIGITS
	    call malloc (iresult, oplen, TY_CHAR)
	    call sprintf (Memc[iresult], SZ_FNAME, "%.2h")
		call pargd (dresult)
	}

	# Write the result to the output operand.  Bool results are stored in
	# iresult as an integer value, string results are stored in iresult as
	# a pointer to the output string, and integer and real/double results
	# are stored in iresult and dresult without any tricks.

	call xvv_initop (out, oplen, optype)

	switch (optype) {
	case TY_BOOL:
	    O_VALI(out) = btoi (iresult != 0)
	case TY_CHAR:
	    O_VALP(out) = iresult
	case TY_INT:
	    O_VALI(out) = iresult
	case TY_REAL:
	    O_VALR(out) = dresult
	case TY_DOUBLE:
	    O_VALD(out) = dresult
	}

free_
	# Free any storage used by the argument list operands.
	do i = 1, nargs
	    call xvv_freeop (args[i])

	call sfree (sp)
	return

badtype_
	call xvv_error1 ("bad argument to function `%s'", fcn)
	call sfree (sp)
	return
end
