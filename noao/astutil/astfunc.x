include	<evvexpr.h>
include	<lexnum.h>
include	<time.h>
include	<mach.h>
include	<imset.h>
include	"astfunc.h"

define	KEYWORDS "|sexstr|epoch|julday|mst|precess|ra_precess|dec_precess|\
		  |airmass|eairmass|obsdb|arcsep|\
		  |if|format|print|printf|error|clget|clput|scan|fscan|\
		  |imget|imput|imdel|"

define	F_SEXSTR		1	# sexstr (value)
define	F_EPOCH			2	# epoch (date[, ut])
define	F_JULDAY		3	# julday (date[, ut])
define	F_MST			4	# mst (date[, ut], longitude)
define	F_PRECESS		5	# precess (ra, dec, epoch1, epoch2)
define	F_RAPRECESS		6	# ra_precess (ra, dec, epoch1, epoch2)
define	F_DECPRECESS		7	# dec_precess (ra, dec, epoch1, epoch2)
define	F_AIRMASS		9	# airmass (ra, dec, st, latitude)
define	F_EAIRMASS		10	# eairmass (ra, dec, st, exptime, lat)
define	F_OBSDB			11	# obsdb (observatory, parameter)
define	F_ARCSEP		12	# arcsep (ra1, dec1, ra2, dec2)

define	F_IOFUNC		13	# Misc && I/O functions after here.
define	F_IF			14	# if (arg)
define	F_FORMAT		15	# format (fmt, arg, ...)
define	F_PRINT			16	# print (arg, ...)
define	F_PRINTF		17	# printf (fmt, arg, ...)
define	F_ERROR			18	# error (message)
define	F_CLGET			19	# clget (name)
define	F_CLPUT			20	# clput (name, value)
define	F_SCAN			21	# scan (params)
define	F_FSCAN			22	# fscan (params)
define	F_IMGET			24	# imget (keyword)
define	F_IMPUT			25	# imput (keyword, value)
define	F_IMDEL			26	# imdelete (keyword)

define  SOLTOSID        (($1)*1.00273790935d0)

# AST_FUNC -- Special astronomical functions.

procedure ast_func (ast, func, args, nargs, out)

pointer	ast			#I client data
char	func[ARB]		#I function to be called
pointer	args[ARB]		#I pointer to arglist descriptor
int	nargs			#I number of arguments
pointer	out			#O output operand (function value)

int	yr, mo, day
double	time, epoch, ra, dec, longitude, latitude
double	ast_julday(), ast_mst(), airmass()

double	dresult
int	iresult, optype, oplen, opcode, v_nargs, i, ip, flags
pointer	sp, buf, dval, obs

bool	strne()
pointer	obsopen()
double	ast_arcsep()
int	strdic(), ctod(), btoi(), dtm_decode()
errchk	malloc, obsopen, obsgstr

begin
	call smark (sp)
	call salloc (buf, SZ_LINE, TY_CHAR)
	call salloc (dval, nargs, TY_DOUBLE)

	# Lookup the function name in the dictionary.  An exact match is
	# required (strdic permits abbreviations).  Abort if the function
	# is not known.

	opcode = strdic (func, Memc[buf], SZ_LINE, KEYWORDS)
	if (opcode == 0 || strne (func, Memc[buf]))
	    call xvv_error1 ("unknown function `%s' called", func)

	if (opcode > F_IOFUNC) {
	    call sfree (sp)
	    call ast_iofunc (ast, func, args, nargs, out)
	    return
	}

	# Verify correct number of arguments.
	switch (opcode) {
	case F_SEXSTR:
	    v_nargs = -1
	case F_EPOCH, F_JULDAY:
	    v_nargs = -1
	case F_MST:
	    v_nargs = -2
	case F_PRECESS, F_RAPRECESS, F_DECPRECESS:
	    v_nargs = 4
	case F_AIRMASS:
	    v_nargs = 4
	case F_EAIRMASS:
	    v_nargs = 5
	case F_OBSDB:
	    v_nargs = 2
	case F_ARCSEP:
	    v_nargs = 4
	default:
	    v_nargs = 1
	}

	if (v_nargs > 0 && nargs != v_nargs)
	    call xvv_error2 ("function `%s' requires %d arguments",
		func, v_nargs)
	else if (v_nargs < 0 && nargs < abs(v_nargs))
	    call xvv_error2 ("function `%s' requires at least %d arguments",
		func, abs(v_nargs))

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
	    if (dtm_decode (O_VALC(args[1]), yr, mo, day, time, flags) == ERR)
		call xvv_error ("unrecognized date format")
	    switch (opcode) {
	    case F_EPOCH, F_JULDAY:
		if (nargs > 1)
		    time = Memd[dval+1]
	    case F_MST:
		if (nargs > 2)
		    time = Memd[dval+1]
	    }
	    if (IS_INDEFD(time))
		time = 0.
	    call ast_date_to_epoch (yr, mo, day, time, epoch)
	}

	# Evaluate the function.
	oplen = 0
	optype = TY_DOUBLE
	switch (opcode) {
	case F_SEXSTR:
	    optype = TY_CHAR
	    oplen = MAX_DIGITS
	    call malloc (iresult, oplen, TY_CHAR)
	    call sprintf (Memc[iresult], oplen, "%.*h")
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
	    longitude = Memd[dval+nargs-1]
	    dresult = ast_mst (epoch, longitude)

	case F_PRECESS:
	    call ast_precess (Memd[dval], Memd[dval+1], Memd[dval+2],
		ra, dec, Memd[dval+3])

	    optype = TY_CHAR
	    oplen = SZ_LINE
	    call malloc (iresult, oplen, TY_CHAR)
	    call sprintf (Memc[iresult], oplen, "%11.2h %11.1h %7g")
		call pargd (ra)
		call pargd (dec)
		call pargd (Memd[dval+3])

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

	case F_ARCSEP:
	    dresult = ast_arcsep (Memd[dval], Memd[dval+1], Memd[dval+2],
		Memd[dval+3])

	default:
	    call xvv_error ("bad switch in user function")
	}

	# Format sexigesimal strings.
	switch (opcode) {
	case F_MST, F_RAPRECESS, F_DECPRECESS:
	    optype = TY_CHAR
	    oplen = MAX_DIGITS
	    call malloc (iresult, oplen, TY_CHAR)
	    call sprintf (Memc[iresult], oplen, "%.2h")
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

	# Free any storage used by the argument list operands.
	do i = 1, nargs
	    call xvv_freeop (args[i])

	call sfree (sp)
	return
end


# AST_IOFUNC -- I/O and miscellaneous functions.

procedure ast_iofunc (ast, func, args, nargs, out)

pointer	ast			#I client data
char	func[ARB]		#I function to be called
pointer	args[ARB]		#I pointer to arglist descriptor
int	nargs			#I number of arguments
pointer	out			#O output operand (function value)

double	dresult
int	iresult, optype, oplen, opcode, v_nargs, i, j, k, l, ip
pointer	sp, buf, sym, im

double	imgetd()
bool	strne()
pointer	stfind(), stenter()
int	strdic(), ctoi(), ctod(), btoi()
int	fscan(), nscan(), lexnum()
int	imaccf(), imgftype(), imgeti()
errchk	malloc

begin
	call smark (sp)
	call salloc (buf, SZ_LINE, TY_CHAR)

	# Lookup the function name in the dictionary.  An exact match is
	# required (strdic permits abbreviations).  Abort if the function
	# is not known.

	opcode = strdic (func, Memc[buf], SZ_LINE, KEYWORDS)
	if (opcode == 0 || strne (func, Memc[buf]))
	    call xvv_error1 ("unknown function `%s' called", func)

	# Verify correct number of arguments.
	switch (opcode) {
	case F_IF:
	    v_nargs = 1
	case F_FORMAT, F_PRINTF:
	    v_nargs = -1
	case F_PRINT:
	    v_nargs = 0
	case F_ERROR:
	    v_nargs = 1
	case F_CLGET:
	    v_nargs = 1
	case F_CLPUT:
	    v_nargs = 2
	case F_SCAN:
	    v_nargs = -1
	case F_FSCAN:
	    v_nargs = 0
	case F_IMGET, F_IMDEL:
	    v_nargs = 1
	case F_IMPUT:
	    v_nargs = 2
	default:
	    v_nargs = 1
	}

	if (v_nargs > 0 && nargs != v_nargs)
	    call xvv_error2 ("function `%s' requires %d arguments",
		func, v_nargs)
	else if (v_nargs < 0 && nargs < abs(v_nargs))
	    call xvv_error2 ("function `%s' requires at least %d arguments",
		func, abs(v_nargs))

	# Evaluate the function.
	oplen = 0
	optype = TY_INT
	iresult = 0
	switch (opcode) {
	case F_IF:
	    optype = TY_BOOL
	    switch (O_TYPE(args[1])) {
	    case TY_BOOL:
		iresult = O_VALI(args[1])
	    case TY_INT:
		iresult = btoi (O_VALI(args[1]) != 0)
	    case TY_REAL:
		iresult = btoi (O_VALR(args[1]) != 0.)
	    case TY_DOUBLE:
		iresult = btoi (O_VALD(args[1]) != 0D0)
	    case TY_CHAR:
		iresult = btoi (O_VALC(args[1])=='y' || O_VALC(args[1])=='Y')
	    }

	case F_FORMAT:
	    optype = TY_CHAR
	    oplen = SZ_LINE
	    call malloc (iresult, oplen, TY_CHAR)
	    call sprintf (Memc[iresult], oplen, O_VALC(args[1]))
	    do i = 2, nargs {
		switch (O_TYPE(args[i])) {
		case TY_CHAR:
		    call pargstr (O_VALC(args[i]))
		case TY_INT:
		    call pargi (O_VALI(args[i]))
		case TY_REAL:
		    call pargr (O_VALR(args[i]))
		case TY_DOUBLE:
		    call pargd (O_VALD(args[i]))
		}
	    }

	case F_PRINT:
	    do i = 1, nargs {
		switch (O_TYPE(args[i])) {
		case TY_CHAR:
		    call printf ("%s")
			call pargstr (O_VALC(args[i]))
		case TY_INT:
		    call printf ("%d")
			call pargi (O_VALI(args[i]))
		case TY_REAL:
		    call printf ("%g")
			call pargr (O_VALR(args[i]))
		case TY_DOUBLE:
		    call printf ("%g")
			call pargd (O_VALD(args[i]))
		}
		if (i < nargs)
		    call printf (" ")
	    }
	    call printf ("\n")

	case F_PRINTF:
	    if (O_TYPE(args[1]) != TY_CHAR)
		call xvv_error ("invalid print format")
	    call printf (O_VALC(args[1]))
	    do i = 2, nargs {
		switch (O_TYPE(args[i])) {
		case TY_CHAR:
		    call pargstr (O_VALC(args[i]))
		case TY_INT:
		    call pargi (O_VALI(args[i]))
		case TY_REAL:
		    call pargr (O_VALR(args[i]))
		case TY_DOUBLE:
		    call pargd (O_VALD(args[i]))
		}
	    }

	case F_ERROR:
	    if (O_TYPE(args[1]) != TY_CHAR)
		call xvv_error ("error function requires string argument")
	    call xvv_error (O_VALC(args[1]))

	case F_CLGET:
	    if (O_TYPE(args[1]) != TY_CHAR)
		call xvv_error ("clget function requires a string argument")
	    call clgstr (O_VALC(args[1]), Memc[buf], SZ_LINE)
	    ip = 1
	    i = lexnum (Memc[buf], ip, j)
	    if (Memc[buf+j] != EOS)
		i = LEX_NONNUM
	    switch (i) {
	    case LEX_OCTAL, LEX_DECIMAL, LEX_HEX:
		ip = 1
		optype = TY_INT
		k = ctoi (Memc[buf], ip, iresult)
	    case LEX_REAL:
		ip = 1
		optype = TY_DOUBLE
		k = ctod (Memc[buf], ip, dresult)
	    default:
		optype = TY_CHAR
		oplen = SZ_LINE
		call malloc (iresult, oplen, TY_CHAR)
		call strcpy (Memc[buf], Memc[iresult], oplen)
	    }

	case F_CLPUT:
            switch (O_TYPE(args[2])) {
            case TY_BOOL:
		call clputb (O_VALC(args[1]), (O_VALI(args[2]) == YES))
            case TY_CHAR:
		call clpstr (O_VALC(args[1]), O_VALC(args[2]))
            case TY_INT:
		call clputi (O_VALC(args[1]), O_VALI(args[2]))
            case TY_REAL:
		call clputr (O_VALC(args[1]), O_VALR(args[2]))
            case TY_DOUBLE:
		call clputd (O_VALC(args[1]), O_VALD(args[2]))
	    }

	case F_SCAN, F_FSCAN:
	    if (opcode == F_FSCAN) {
		if (AST_TFD(ast) == NULL)
		    call xvv_error ("no text file for fscan")
		if (fscan (AST_TFD(ast)) == EOF)
		    call xvv_error ("end of text file in fscan")
		l = 1
	    } else {
		if (O_TYPE(args[1]) != TY_CHAR)
		    call xvv_error ("scan function requires string argument")
		call sscan (O_VALC(args[1]))
		l = 2
	    }
	    do i = l, nargs {
		call gargwrd (Memc[buf], SZ_LINE)
		if (nscan() != i-l+1)
		    break
		sym = stfind (AST_STP(ast), O_VALC(args[i]))
		if (sym == NULL)
		    sym = stenter (AST_STP(ast), O_VALC(args[i]), SZ_LINE)

		ip = 1
		j = lexnum (Memc[buf], ip, k)
		if (Memc[buf+k] != EOS)
		    j = LEX_NONNUM
		switch (j) {
		case LEX_OCTAL, LEX_DECIMAL, LEX_HEX:
		    ip = 1
		    Memi[sym] = TY_INT
		    k = ctoi (Memc[buf], ip, Memi[sym+2])
		case LEX_REAL:
		    ip = 1
		    Memi[sym] = TY_DOUBLE
		    k = ctod (Memc[buf], ip, Memd[P2D(sym+2)])
		default:
		    Memi[sym] = TY_CHAR
		    call strcpy (Memc[buf], Memc[P2C(sym+2)], SZ_LINE)
		}
	    }

	    optype = TY_INT
	    iresult = nscan()

	case F_IMGET:
	    im = AST_IM(ast)
	    if (im == NULL)
		call xvv_error ("no image for imget")
	    if (imaccf (im, O_VALC(args[1])) == YES) {
		switch (imgftype (im, O_VALC(args[1]))) {
		case TY_BOOL, TY_SHORT, TY_INT, TY_LONG:
		    optype = TY_INT
		    iresult = imgeti (im, O_VALC(args[1]))
		case TY_REAL, TY_DOUBLE, TY_COMPLEX:
		    optype = TY_DOUBLE
		    dresult = imgetd (im, O_VALC(args[1]))
		default:
		    optype = TY_CHAR
		    oplen = SZ_LINE
		    call malloc (iresult, oplen, TY_CHAR)
		    call imgstr (im, O_VALC(args[1]), Memc[iresult], oplen)
		}
	    } else {
		optype = TY_CHAR
		oplen = 1
		call calloc (iresult, oplen, TY_CHAR)
	    }

	case F_IMPUT:
	    im = AST_IM(ast)
	    if (im == NULL)
		call xvv_error ("no image for imput")

	    optype = TY_CHAR
	    oplen = SZ_LINE
	    call malloc (iresult, oplen, TY_CHAR)
	    iferr (call imgstr (im, O_VALC(args[1]), Memc[buf], SZ_LINE)) {
		call sprintf (Memc[iresult], oplen, "imput: %s = ")
		    call pargstr (O_VALC(args[1]))
	    } else {
		call sprintf (Memc[iresult], oplen, "imput: %s = %s -> ")
		    call pargstr (O_VALC(args[1]))
		    call pargstr (Memc[buf])
	    }

	    iferr (call imdelf (im, O_VALC(args[1])))
		;
            switch (O_TYPE(args[2])) {
            case TY_BOOL:
                call imaddb (im, O_VALC(args[1]), (O_VALI(args[2]) == YES))
            case TY_CHAR:
                call imastr (im, O_VALC(args[1]), O_VALC(args[2]))
            case TY_INT:
                call imaddi (im, O_VALC(args[1]), O_VALI(args[2]))
            case TY_REAL:
                call imaddr (im, O_VALC(args[1]), O_VALR(args[2]))
            case TY_DOUBLE:
                call imaddd (im, O_VALC(args[1]), O_VALD(args[2]))
	    }

	    call imgstr (im, O_VALC(args[1]), Memc[buf], SZ_LINE)
	    call strcat (Memc[buf], Memc[iresult], oplen)

	case F_IMDEL:
	    im = AST_IM(ast)
	    if (im == NULL)
		call xvv_error ("no image for imdelete")

	    optype = TY_CHAR
	    oplen = SZ_LINE
	    call malloc (iresult, oplen, TY_CHAR)
	    iferr (call imgstr (im, O_VALC(args[1]), Memc[buf], SZ_LINE)) {
		call sprintf (Memc[iresult], oplen, "imdel: %s not found")
		    call pargstr (O_VALC(args[1]))
	    } else {
		call sprintf (Memc[iresult], oplen, "imdel: %s = %s (DELETED)")
		    call pargstr (O_VALC(args[1]))
		    call pargstr (Memc[buf])
		call imdelf (im, O_VALC(args[1]))
	    }

	default:
	    call xvv_error ("bad switch in user function")
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

	# Free any storage used by the argument list operands.
	do i = 1, nargs
	    call xvv_freeop (args[i])

	call sfree (sp)
	return
end
