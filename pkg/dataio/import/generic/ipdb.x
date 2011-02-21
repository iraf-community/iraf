include <evvexpr.h>
include <error.h>
include <mach.h>
include <imhdr.h>
include "../import.h"
include "../ipfcn.h"

define	DEBUG	false


# IP_EVAL_DBREC -- For each of the keywords defined in the database record,
# evaluate the expression and load the task structure.

procedure ip_eval_dbrec (ip)

pointer	ip					#i task struct pointer

int	ival
pointer	sp, dims, pixtype, err
pointer	np, stp, sym

pointer	stname(), sthead(), stnext
int	or(), ip_dbgeti()
bool	streq()

errchk	ip_dbgeti()

begin
	call smark (sp)
	call salloc (dims, SZ_EXPR, TY_CHAR)
	call salloc (pixtype, SZ_EXPR, TY_CHAR)
	call salloc (err, SZ_EXPR, TY_CHAR)
	call aclrc (Memc[dims], SZ_EXPR)
	call aclrc (Memc[pixtype], SZ_EXPR)
	call aclrc (Memc[err], SZ_EXPR)

	# Load the defaults.
	call ip_load_defaults (ip)

	# First thing we do is get the byte swap flag so the remaining 
	# fields will be interpreted correctly.
	ifnoerr (ival = ip_dbgeti (ip, "bswap"))
	    IP_SWAP(ip) = ival

	# Next, we handle 'interleave', 'dims' and 'pixtype' as a special case
	# since for band- and line-interleaved files we may need to fix up the
	# pixtype pointers.
	ifnoerr (ival = ip_dbgeti (ip, "interleave"))
	    IP_INTERLEAVE(ip) = ival

	ifnoerr (call ip_dbstr (ip, "dims", Memc[dims], SZ_EXPR))
	    call ip_do_dims (ip, Memc[dims])

	ifnoerr (call ip_dbstr (ip, "pixtype", Memc[pixtype], SZ_EXPR)) {
	    if (Memc[pixtype] == '"')
	        call fdb_strip_quote (Memc[pixtype], Memc[pixtype], SZ_EXPR)
	    call ip_do_pixtype (ip, Memc[pixtype])
	}

	# Loop over every symbol in the table.
	stp = IP_FSYM(ip)
        for (sym=sthead(stp);  sym != NULL;  sym=stnext(stp,sym)) {
	    np = stname (stp, sym)

	    if (streq(Memc[np],"format") ||	# ignored or found already
		streq(Memc[np],"alias") ||
		streq(Memc[np],"image_id") ||
		streq(Memc[np],"interleave") ||
		streq(Memc[np],"dims") ||
		streq(Memc[np],"pixtype") ||
		streq(Memc[np],"id_string") ||
		streq(Memc[np],"bswap")) {
		    next
	    } else if (streq(Memc[np],"hskip")) {
		IP_HSKIP(ip) = ip_dbgeti (ip, "hskip")
	    } else if (streq(Memc[np],"tskip")) {
		IP_TSKIP(ip) = ip_dbgeti (ip, "tskip")
	    } else if (streq(Memc[np],"bskip")) {
		IP_BSKIP(ip) = ip_dbgeti (ip, "bskip")
	    } else if (streq(Memc[np],"lskip")) {
		IP_LSKIP(ip) = ip_dbgeti (ip, "lskip")
	    } else if (streq(Memc[np],"lpad")) {
		IP_LPAD(ip) = ip_dbgeti (ip, "lpad")
	    } else if (streq(Memc[np],"yflip")) {
		if (ip_dbgeti (ip, "yflip") == YES)
		    IP_FLIP(ip) = or (IP_FLIP(ip), FLIP_Y)
	    } else if (streq(Memc[np],"error")) {
		if (IP_OUTPUT(ip) != IP_INFO)
		    call ip_do_error (ip, Memc[P2C(sym)])
	    } else if (streq(Memc[np],"comment")) {
		call fdb_strip_quote (Memc[P2C(sym)], Memc[P2C(sym)], SZ_LINE)
		call ip_do_comment (ip, Memc[P2C(sym)])
	    } else {
		call eprintf ("Warning: Unknown database keyword '%s'.\n")
		    call pargstr (Memc[np])
	    } 
	}

	if (DEBUG) { call zzi_prstruct ("eval dbrec:", ip) }
	call sfree (sp)
end


# IP_LOAD_DEFAULTS -- Load the default input parameters to the task structure.

procedure ip_load_defaults (ip)

pointer	ip					#i task struct pointer

begin
        IP_SWAP(ip)       = DEF_SWAP            # type of byte swapping
        IP_INTERLEAVE(ip) = DEF_INTERLEAVE      # type of data interleaving
        IP_HSKIP(ip)      = DEF_HSKIP           # bytes to skip before data
        IP_TSKIP(ip)      = DEF_TSKIP           # bytes to skip after data
        IP_BSKIP(ip)      = DEF_BSKIP           # bytes between image bands
        IP_LSKIP(ip)      = DEF_LSKIP           # bytes to skip at front of
        IP_LPAD(ip)       = DEF_LPAD            # bytes to skip at end of

	# zero image dimensions
	for (IP_NDIM(ip)=IM_MAXDIM; IP_NDIM(ip) > 0; IP_NDIM(ip)=IP_NDIM(ip)-1)
	    IP_AXLEN(ip,IP_NDIM(ip)) = 0
end


# IP_DBFCN -- Called by evvexpr to execute format database special functions.

procedure ip_dbfcn (ip, fcn, args, nargs, o)

pointer	ip					#i task struct pointer
char	fcn[ARB]				#i function to be executed
pointer	args[ARB]				#i argument list
int	nargs					#i number of arguments
pointer	o					#o operand pointer

pointer	sp, buf, outstr
int	fd, func, v_nargs
int	i, len, nchar, ival, cur_offset, swap
char	ch
short	sval
real	rval
double	dval

short	ip_getb(), ip_gets()
int	strdic(), ip_line(), ip_locate(), ip_getu()
int	ctoi(), ctol(), ctor(), ctod(), ctocc(), ctowrd()
int	and(), strlen(), clgeti()
long	ip_getl()
real	ip_getr(), ip_getn()
double	ip_getd(), ip_getn8()
bool	strne(), streq()

begin
	call smark (sp)
	call salloc (buf, SZ_LINE, TY_CHAR)
	call salloc (outstr, SZ_LINE, TY_CHAR)
	call aclrc (Memc[buf], SZ_LINE)
	call aclrc (Memc[outstr], SZ_LINE)

	# Lookup function in dictionary.
	func = strdic (fcn, Memc[buf], SZ_LINE, DB_FUNCTIONS)
        if (func > 0 && strne(fcn,Memc[buf]))
            func = 0

        # Abort if the function is not known.
        if (func <= 0)
            call xev_error1 ("unknown function `%s' called", fcn)


	# Verify the correct number of arguments, negative value means a
	# variable number of args, handle it in the evaluation.
	switch (func) {
	case CTOCC, CTOD, CTOI, CTOL, CTOR, CTOWRD:
	    v_nargs = -1

	case GETSTR:
	    v_nargs = -1
	case GETB, GETU, GETI, GETI2, GETI4, GETR, GETR4, GETR8, 
	     GETN, GETN4, GETN8:
	        v_nargs = 1

	case LOCATE:
	    v_nargs = -1
	case LINE, SKIP:
	    v_nargs = 1

	case BSWAP:
	    v_nargs = 1
	case PARAMETER, DEFAULT:
	    v_nargs = 1
	case SUBSTR:
	    v_nargs = 3
	case STRIDX:
	    v_nargs = 2
	case LSB_HOST, MSB_HOST:
	    v_nargs = 0
	}
        if (v_nargs > 0 && nargs != v_nargs)
            call xev_error2 ("function `%s' requires %d arguments",
                fcn, v_nargs)
        else if (v_nargs < 0 && nargs < abs(v_nargs))
            call xev_error2 ("function `%s' requires at least %d arguments",
                fcn, abs(v_nargs))

	fd = IP_FD(ip)
	swap = IP_SWAP(ip)
	cur_offset = IP_OFFSET(ip)

	if (DEBUG) { 
	    call eprintf ("cur_offset=%d nargs=%d func=%s swap=%d\n")
		call pargi(cur_offset) ; call pargi(nargs)
		call pargstr(fcn) ; call pargi (swap)
	    do i = 1, nargs
	        call zzi_pevop (args[i])
	    call eprintf ("init op =>  ") ; call zzi_pevop(o)

	}

	# Evaluate the function.
	switch (func) {
        case CTOCC:		# run the fmtio equivalents of the argument
	    if (nargs == 1)
	        ch = ip_getb (fd, O_VALI(args[1]))
	    else
	        ch = ip_getb (fd, cur_offset)
	    len = ctocc (ch, Memc[outstr],  SZ_FNAME) + 1
	    call ip_initop (o, len, TY_CHAR)
	    call aclrc (O_VALC(o), len)
	    call amovc (Memc[outstr], O_VALC(o), len)
	    cur_offset = cur_offset + 1
	    call ip_lseek (fd, cur_offset)

        case CTOWRD:
	    if (nargs == 1)
	        call ip_gstr (fd, O_VALI(args[1]), SZ_FNAME, Memc[outstr])
	    else
	        call ip_gstr (fd, cur_offset, SZ_FNAME, Memc[outstr])
	    nchar = ctowrd (Memc[outstr], i, Memc[outstr],  SZ_FNAME) + 1
	    call ip_initop (o, nchar, TY_CHAR)
	    call aclrc (O_VALC(o), nchar)
	    call amovc (Memc[outstr], O_VALC(o), nchar)
	    cur_offset = cur_offset + nchar + 1
	    call ip_lseek (fd, cur_offset)

        case CTOI:
	    i = 1
	    if (nargs == 1) {
	        call ip_gstr (fd, O_VALI(args[i]), SZ_FNAME, Memc[outstr])
		nchar = ctoi (Memc[outstr], i, ival)
	        cur_offset = cur_offset + nchar - 1
	    } else if (nargs == 2) {
	        call ip_gstr (fd, O_VALI(args[1]), O_VALI(args[2]),Memc[outstr])
		nchar = ctoi (Memc[outstr], i, ival)
	        cur_offset = O_VALI(args[1]) + nchar - 1
	    }
	    call ip_lseek (fd, cur_offset)
	    O_TYPE(o) = TY_INT

        case CTOL:
	    i = 1
	    if (nargs == 1) {
	        call ip_gstr (fd, O_VALI(args[i]), SZ_FNAME, Memc[outstr])
		nchar = ctol (Memc[outstr], i, ival)
	        cur_offset = cur_offset + nchar - 1
	    } else if (nargs == 2) {
	        call ip_gstr (fd, O_VALI(args[1]), O_VALI(args[2]),Memc[outstr])
		nchar = ctol (Memc[outstr], i, ival)
	        cur_offset = O_VALI(args[1]) + nchar - 1
	    }
	    call ip_lseek (fd, cur_offset)
	    O_TYPE(o) = TY_LONG

        case CTOR:
	    i = 1
	    if (nargs == 1) {
	        call ip_gstr (fd, O_VALI(args[i]), SZ_FNAME, Memc[outstr])
		nchar = ctor (Memc[outstr], i, rval)
	        cur_offset = cur_offset + nchar - 1
	    } else if (nargs == 2) {
	        call ip_gstr (fd, O_VALI(args[1]), O_VALI(args[2]),Memc[outstr])
		nchar = ctor (Memc[outstr], i, rval)
	        cur_offset = O_VALI(args[1]) + nchar - 1
	    }
	    call ip_lseek (fd, cur_offset)
	    O_TYPE(o) = TY_REAL

        case CTOD:
	    i = 1
	    if (nargs == 1) {
	        call ip_gstr (fd, O_VALI(args[i]), SZ_FNAME, Memc[outstr])
		nchar = ctod (Memc[outstr], i, dval)
	        cur_offset = cur_offset + nchar - 1
	    } else if (nargs == 2) {
	        call ip_gstr (fd, O_VALI(args[1]), O_VALI(args[2]),Memc[outstr])
		nchar = ctod (Memc[outstr], i, dval)
	        cur_offset = O_VALI(args[1]) + nchar - 1
	    }
	    call ip_lseek (fd, cur_offset)
	    O_TYPE(o) = TY_DOUBLE

        case GETSTR:
	    if (nargs == 1) {
		call ip_gstr (fd, cur_offset, O_VALI(args[1]), Memc[outstr])
	        cur_offset = cur_offset + O_VALI(args[1])
	    } else if (nargs == 2) {
		call ip_gstr (fd, O_VALI(args[1]), O_VALI(args[2]),Memc[outstr])
	        cur_offset = O_VALI(args[1]) + O_VALI(args[2]) - 1
	    }
	    if (strlen(Memc[outstr]) == 0) {
		len = strlen ("ERR") + 1
	        call ip_initop (o, len, TY_CHAR)
		call aclrc (O_VALC(o), len)
	        call strcpy ("ERR", O_VALC(o), len-1)
	    } else {
		len = strlen (Memc[outstr]) + 1
	        call ip_initop (o, len, TY_CHAR)
		call aclrc (O_VALC(o), len)
	        call amovc (Memc[outstr], O_VALC(o), len-1)
	    }

        case GETB:
	    if (nargs == 0) {
		sval = ip_getb (fd, cur_offset)
	        cur_offset = cur_offset + SZB_CHAR
	    } else {
		sval = ip_getb (fd, O_VALI(args[1]))
	        cur_offset = O_VALI(args[1]) + SZB_CHAR
	    }
	    ival = sval
	    O_TYPE(o) = TY_INT

        case GETU:
	    if (nargs == 0) {
		sval = short (ip_getu (fd, cur_offset))
	        cur_offset = cur_offset + (SZB_CHAR * SZ_SHORT)
	    } else {
		sval = short (ip_getu (fd, O_VALI(args[1])))
	        cur_offset = O_VALI(args[1]) + (SZB_CHAR * SZ_SHORT)
	    }
	    if (and(swap, S_ALL) == S_ALL || and(swap, S_I2) == S_I2)
	    	call bswap2 (sval, 1, sval, 1, (SZ_SHORT*SZB_CHAR))
	    ival = sval
	    O_TYPE(o) = TY_INT

        case GETI, GETI2:
	    if (nargs == 0) {
		sval = ip_gets (fd, cur_offset)
	        cur_offset = cur_offset + (SZB_CHAR * SZ_SHORT)
	    } else {
		sval = ip_gets (fd, O_VALI(args[1]))
	        cur_offset = O_VALI(args[1]) + (SZB_CHAR * SZ_SHORT)
	    }
	    if (and(swap, S_ALL) == S_ALL || and(swap, S_I2) == S_I2) 
		call bswap2 (sval, 1, sval, 1, (SZ_SHORT*SZB_CHAR))
	    ival = sval
	    O_TYPE(o) = TY_INT

        case GETI4:
	    if (nargs == 0) {
		ival = ip_getl (fd, cur_offset)
	        cur_offset = cur_offset + (SZB_CHAR * SZ_LONG)
	    } else {
		ival = ip_getl (fd, O_VALI(args[1]))
	        cur_offset = O_VALI(args[1]) + (SZB_CHAR * SZ_LONG)
	    }
	    if (and(swap, S_ALL) == S_ALL || and(swap, S_I2) == S_I4)
		call bswap4 (ival, 1, ival, 1, (SZ_INT32*SZB_CHAR))
	    O_TYPE(o) = TY_INT

        case GETR, GETR4:
	    if (nargs == 0) {
		rval = ip_getr (fd, cur_offset)
	        cur_offset = cur_offset + (SZB_CHAR * SZ_REAL)
	    } else {
		rval = ip_getr (fd, O_VALI(args[1]))
	        cur_offset = O_VALI(args[1]) + (SZB_CHAR * SZ_REAL)
	    }
	    if (and(swap, S_ALL) == S_ALL)	# handle byte-swapping
		call bswap4 (rval, 1, rval, 1, (SZ_REAL*SZB_CHAR))
	    O_TYPE(o) = TY_REAL

        case GETR8:
	    if (nargs == 0) {
		dval = ip_getd (fd, cur_offset)
	        cur_offset = cur_offset + (SZB_CHAR * SZ_DOUBLE)
	    } else {
		dval = ip_getd (fd, O_VALI(args[1]))
	        cur_offset = O_VALI(args[1]) + (SZB_CHAR * SZ_DOUBLE)
	    }
	    if (and(swap, S_ALL) == S_ALL)	# handle byte-swapping
		call bswap8 (dval, 1, dval, 1, (SZ_DOUBLE*SZB_CHAR))
	    O_TYPE(o) = TY_DOUBLE

        case GETN, GETN4:
	    if (nargs == 0) {
		rval = ip_getn (fd, cur_offset)
	        cur_offset = cur_offset + (SZB_CHAR * SZ_REAL)
	    } else {
		rval = ip_getn (fd, O_VALI(args[1]))
	        cur_offset = O_VALI(args[1]) + (SZB_CHAR * SZ_REAL)
	    }
	    if (and(swap, S_ALL) == S_ALL)	# handle byte-swapping
		call bswap4 (rval, 1, rval, 1, (SZ_REAL*SZB_CHAR))
	    O_TYPE(o) = TY_REAL

        case GETN8:
	    if (nargs == 0) {
		dval = ip_getn8 (fd, cur_offset)
	        cur_offset = cur_offset + (SZB_CHAR * SZ_DOUBLE)
	    } else {
		dval = ip_getn8 (fd, O_VALI(args[1]))
	        cur_offset = O_VALI(args[1]) + (SZB_CHAR * SZ_DOUBLE)
	    }
	    if (and(swap, S_ALL) == S_ALL)	# handle byte-swapping
		call bswap8 (dval, 1, dval, 1, (SZ_DOUBLE*SZB_CHAR))
	    O_TYPE(o) = TY_DOUBLE

        case LOCATE: 			# locate the pattern in the file
	    if (nargs == 1)
	        ival = ip_locate (fd, cur_offset, O_VALC(args[1]))
	    else if (nargs == 2)
	        ival = ip_locate (fd, O_VALI(args[1]), O_VALC(args[2]))
	    if (ival == ERR)
		ival = 1
	    O_TYPE(o) = TY_INT
	    cur_offset = ival

        case LINE: 			# locate the line no. in the file
	    ival = ip_line (fd, O_VALI(args[1]))
	    if (ival == ERR)
		ival = 1
	    O_TYPE(o) = TY_INT
	    cur_offset = ival

        case SKIP: 			# skip a certain number of bytes
	    ival = O_VALI(args[1])
	    O_TYPE(o) = TY_INT
	    cur_offset = cur_offset + ival

        case BSWAP: 			# byte-swap argument
	    O_TYPE(o) = O_TYPE(args[1])
	    switch (O_TYPE(args[1])) {
	    case TY_SHORT:
		call bswap2 (O_VALS(args[1]), 1, sval, 1, (SZ_SHORT*SZB_CHAR))
	    case TY_INT:
		call bswap4 (O_VALI(args[1]), 1, ival, 1, (SZ_INT32*SZB_CHAR))
	    case TY_LONG:
		call bswap4 (O_VALL(args[1]), 1, ival, 1, (SZ_LONG*SZB_CHAR))
	    case TY_REAL:
		call bswap4 (O_VALR(args[1]), 1, rval, 1, (SZ_REAL*SZB_CHAR))
	    case TY_DOUBLE:
		call bswap8 (O_VALD(args[1]), 1, dval, 1, (SZ_DOUBLE*SZB_CHAR))
	    }

        case PARAMETER: 		# return current task parameter value
	    if (streq(O_VALC(args[1]),"dims")) {
		call clgstr ("dims", Memc[outstr], SZ_FNAME)
		len = strlen (Memc[outstr]) + 1
	        call ip_initop (o, len, TY_CHAR)
	        call strcpy (Memc[outstr], O_VALC(o), len)
	    } else if (streq(O_VALC(args[1]),"pixtype")) {
		call clgstr ("pixtype", Memc[outstr], SZ_FNAME)
		len = strlen (Memc[outstr]) + 1
	        call ip_initop (o, len, TY_CHAR)
	        call strcpy (Memc[outstr], O_VALC(o), len)
	    } else if (streq(O_VALC(args[1]),"interleave")) {
		ival = clgeti ("interleave")
		O_TYPE(o) = TY_INT
	    } else if (streq(O_VALC(args[1]),"bswap")) {
		call clgstr ("bswap", Memc[outstr], SZ_FNAME)
		if (strne("no",Memc[outstr]) && strne("none",Memc[outstr]))
		    ival = YES
		else
		    ival = NO
		O_TYPE(o) = TY_BOOL
	    } else if (streq(O_VALC(args[1]),"hskip")) {
		ival = clgeti ("hskip")
		O_TYPE(o) = TY_INT
	    } else if (streq(O_VALC(args[1]),"tskip")) {
		ival = clgeti ("tskip")
		O_TYPE(o) = TY_INT
	    } else if (streq(O_VALC(args[1]),"bskip")) {
		ival = clgeti ("bskip")
		O_TYPE(o) = TY_INT
	    } else if (streq(O_VALC(args[1]),"lskip")) {
		ival = clgeti ("lskip")
		O_TYPE(o) = TY_INT
	    } else if (streq(O_VALC(args[1]),"lpad")) {
		ival = clgeti ("lpad")
		O_TYPE(o) = TY_INT
	    }

        case DEFAULT: 			# return default task parameter value
	    if (streq(O_VALC(args[1]),"dims")) {
	        call ip_initop (o, 1, TY_CHAR)
	        call strcpy ("", O_VALC(o), 1)
	    } else if (streq(O_VALC(args[1]),"pixtype")) {
	        call ip_initop (o, 1, TY_CHAR)
	        call strcpy ("", O_VALC(o), 1)
	    } else if (streq(O_VALC(args[1]),"interleave")) {
		ival = DEF_INTERLEAVE
		O_TYPE(o) = TY_INT
	    } else if (streq(O_VALC(args[1]),"bswap")) {
		ival = DEF_SWAP
		O_TYPE(o) = TY_INT
	    } else if (streq(O_VALC(args[1]),"hskip")) {
		ival = DEF_HSKIP
		O_TYPE(o) = TY_INT
	    } else if (streq(O_VALC(args[1]),"tskip")) {
		ival = DEF_TSKIP
		O_TYPE(o) = TY_INT
	    } else if (streq(O_VALC(args[1]),"bskip")) {
		ival = DEF_BSKIP
		O_TYPE(o) = TY_INT
	    } else if (streq(O_VALC(args[1]),"lskip")) {
		ival = DEF_LSKIP
		O_TYPE(o) = TY_INT
	    } else if (streq(O_VALC(args[1]),"lpad")) {
		ival = DEF_LPAD
		O_TYPE(o) = TY_INT
	    }

        case LSB_HOST: 			# host is an LSB byte ordered machine
	    if (BYTE_SWAP2 == YES)
	        ival = YES
	    else
	        ival = NO
	    O_TYPE(o) = TY_BOOL

        case MSB_HOST: 			# host is an MSB byte ordered machine
	    if (BYTE_SWAP2 == NO)
	        ival = YES
	    else
	        ival = NO
	    O_TYPE(o) = TY_BOOL

        case SUBSTR: 			# return a substring of the argument

        case STRIDX: 			# return offset of a char w/in str

	}

	# Write result to output operand.
	O_LEN(o) = 0
	switch (O_TYPE(o)) {
	case TY_USHORT, TY_SHORT:
	    O_VALS(o) = sval
	case TY_INT, TY_BOOL:
	    O_VALI(o) = ival
	case TY_LONG:
	    O_VALL(o) = ival
	case TY_REAL:
	    O_VALR(o) = rval
	case TY_DOUBLE:
	    O_VALD(o) = dval
	}

	if (DEBUG) { call eprintf("ip_dbfcn: ") ; call zzi_pevop (o) }

	IP_OFFSET(ip) = cur_offset
	call sfree (sp)
end


# IP_DBSTR -- Get a string valued expression from the database.

procedure ip_dbstr (ip, param, outstr, maxch)

pointer ip                                      #i task struct pointer
char	param[ARB]				#i parameter to evaluate
char	outstr[ARB]				#o result string
int	maxch					#i max length of string

pointer sp, expr, o

int     locpr(), strlen()
pointer evvexpr()
extern  ip_getop(), ip_dbfcn()
errchk  evvexpr

begin
        call smark (sp)
        call salloc (expr, SZ_EXPR, TY_CHAR)
        call aclrc (Memc[expr], SZ_EXPR)

        # Get the requested parameter.
        call aclrc (outstr, SZ_EXPR)
        call fdbgstr (IP_FSYM(ip), param, Memc[expr], SZ_EXPR)
        if (Memc[expr] == EOS)
            call error (1, "FDBGET: Format parameter not found")

	if (DEBUG) { 
	    call eprintf("ip_dbstr: expr='%s' len=%d ");call pargstr(Memc[expr])
		call pargi(strlen(Memc[expr]))
	}

        # Evaluate the expression.
        iferr {
            o = evvexpr (Memc[expr], locpr(ip_getop), ip,
                locpr(ip_dbfcn), ip, EV_RNGCHK)
            if (O_TYPE(o) != TY_CHAR)
                call error (0, "ip_dbstr: Expression must be a string valued")
	    else
                call amovc (O_VALC(o), outstr, (min(strlen(O_VALC(o)),maxch)))
         } then
             call erract (EA_WARN)

	if (DEBUG) { call eprintf ("outstr=:%s:\n") ; call pargstr (outstr) }

	call evvfree (o)
        call sfree (sp)
end



# IP_DBGETI -- Get integer valued format parameter from the database.

int procedure ip_dbgeti (ip, param)

pointer	ip					#i task struct pointer
char	param[ARB]				#i requested parameter

int	val
pointer	sp, expr, o

int	locpr()
pointer	evvexpr()
extern	ip_getop(), ip_dbfcn()
errchk	evvexpr

begin
	call smark (sp)
	call salloc (expr, SZ_EXPR, TY_CHAR)

	# Get the requested parameter.
        call fdbgstr (IP_FSYM(ip), param, Memc[expr], SZ_EXPR)
        if (Memc[expr] == EOS)
	    call error (1, "IP_DBGET: Format parameter not found")

	# Evaluate the expression.
	if (DEBUG) {
	    call eprintf ("ip_dbget: expr='%s'\n")
		call pargstr (Memc[expr])
	    call flush (STDERR)
	}
        iferr {
            o = evvexpr (Memc[expr], locpr(ip_getop), ip, 
		locpr(ip_dbfcn), ip, EV_RNGCHK)
            if (O_TYPE(o) == TY_BOOL) {
		val = O_VALI(o)
	    } else if (O_TYPE(o) != TY_INT && O_TYPE(o) != TY_SHORT) {
                call error (0, "Expression must be an integer")
	    } else
		val = O_VALI(o)

	    if (DEBUG) {
	        call eprintf ("ip_dbget: val=%d type=%d ecpr=:%s:\n")
		    call pargi (val)
		    call pargi (O_TYPE(o))
		    call pargstr (Memc[expr])
	        call flush (STDERR)
	    }
        } then 
             call erract (EA_WARN)

	call evvfree (o)
	call sfree (sp)
	return (val)
end


# IP_DBGETR -- Get real valued format parameter from the database.

real procedure ip_dbgetr (ip, param)

pointer	ip					#i task struct pointer
char	param[ARB]				#i requested parameter

real	val
pointer	sp, expr, o

int	locpr()
pointer	evvexpr()
extern	ip_getop(), ip_dbfcn()
errchk	evvexpr

begin
	call smark (sp)
	call salloc (expr, SZ_EXPR, TY_CHAR)

	# Get the requested parameter.
        call fdbgstr (IP_FSYM(ip), param, Memc[expr], SZ_EXPR)
        if (Memc[expr] == EOS)
	    call error (1, "IP_DBGET: Format parameter not found")

	# Evaluate the expression.
	if (DEBUG) {
	    call eprintf ("ip_dbget: expr='%s'\n")
		call pargstr (Memc[expr])
	    call flush (STDERR)
	}
        iferr {
            o = evvexpr (Memc[expr], locpr(ip_getop), ip, 
		locpr(ip_dbfcn), ip, EV_RNGCHK)
            if (O_TYPE(o) == TY_BOOL) {
		val = O_VALI(o)
	    } else if (O_TYPE(o) != TY_REAL) {
                call error (0, "Expression must be a real")
	    } else
		val = O_VALR(o)

	    if (DEBUG) {
	        call eprintf ("ip_dbget: val=%d type=%d ecpr=:%s:\n")
		    call pargr (val)
		    call pargi (O_TYPE(o))
		    call pargstr (Memc[expr])
	        call flush (STDERR)
	    }
        } then 
             call erract (EA_WARN)

	call evvfree (o)
	call sfree (sp)
	return (val)
end


# IP_DO_ERROR -- Process the error parameter.

procedure ip_do_error (ip, expr)

pointer ip                                      #i task struct pointer
char    expr[ARB]                               #i error string

pointer o

int     locpr()
pointer evvexpr()
extern  ip_getop(), ip_dbfcn()
bool    strne()
errchk  evvexpr

begin
        if (DEBUG) {call eprintf ("error expr: '%s'  ") ; call pargstr (expr)}

        # Evaluate the expression.
        iferr {
            o = evvexpr (expr, locpr(ip_getop), ip, locpr(ip_dbfcn), ip, 
		EV_RNGCHK)

            if (DEBUG) { call eprintf("-> '%s'\n") ; call pargstr(O_VALC(o)) }

            if (O_TYPE(o) != TY_CHAR)
                call error (2, "do_error: Expression must be a string valued")
            else {
                if (strne("okay",O_VALC(o)))
                    call error (2, O_VALC(o))
            }
            call evvfree (o)

         } then
            if (IP_OUTPUT(ip) != IP_INFO)
                 call erract (EA_FATAL)
end


# IP_DO_COMMENT - Process a comment line in the format database.

procedure ip_do_comment (ip, comstr)

pointer	ip					#i task struct pointer
char	comstr[ARB]				#i comment to add

pointer	sp, buf

begin
	# Copy the comment line to the comment block.
	if (IP_COMPTR(ip) == NULL)
	    call calloc (IP_COMPTR(ip), SZ_COMMENT, TY_CHAR)

	if (COMMENT(ip) == '\0') {
	    call strcpy ("\t", COMMENT(ip), SZ_LINE)
	    call strcat (comstr, COMMENT(ip), SZ_LINE)
	} else { 
	    call smark (sp)
	    call salloc (buf, SZ_LINE, TY_CHAR)

	    Memc[buf] = '\0'
	    call strcpy ("\t", Memc[buf], SZ_LINE)
	    call strcat (comstr, Memc[buf], SZ_LINE)
	    call strcat ("\n", Memc[buf], SZ_LINE)
	    call strcat (COMMENT(ip), Memc[buf], SZ_COMMENT)

	    call strcpy (Memc[buf], COMMENT(ip), SZ_COMMENT)

	    call sfree (sp)
	}
end


# IP_INITOP - Initialize an operand pointer to the requested values

procedure ip_initop (o, len, type)

pointer	o				#u operand pointer
int	len				#i length of array
int	type				#i data type of operand

begin
	O_LEN(o) = len
	O_TYPE(o) = type
	if (len > 1)
	    call calloc (O_VALP(o), len, type)
end
