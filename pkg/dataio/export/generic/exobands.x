include <error.h>
include <mach.h>
include <evvexpr.h>
include <fset.h>
include <ctype.h>
include "../export.h"
include "../exfcn.h"

define	DEBUG	false
define	VDEBUG	false


# EX_EVALUATE -- Evaluate the outbands expression.

pointer procedure ex_evaluate (ex, expr)

pointer	ex				#i task struct pointer
char	expr[ARB]			#i expression to be evaluated

pointer	o				# operand pointer to result

int     locpr()
pointer evvexpr()
extern  ex_getop(), ex_obfcn()
errchk  evvexpr

begin
	if (DEBUG) { call eprintf("ex_eval: expr='%s'\n") ; call pargstr(expr) }

        # Evaluate the expression.
        iferr {
            o = evvexpr (expr, locpr(ex_getop), ex, locpr(ex_obfcn), ex, 
		EV_RNGCHK)
         } then
             call erract (EA_FATAL)

	return (o)
end


# EX_GETOP -- Called by evvexpr to get an operand.  

procedure ex_getop (ex, opname, o)

pointer	ex				#i task struct pointer
char	opname[ARB]			#i operand name to retrieve
pointer	o				#o output operand pointer

int	i, nops, found, optype, imnum
pointer	sp, buf
pointer	op, param, emsg
pointer	im

#int	ex_ptype()
int	imgeti(), imgftype(), btoi(), ctoi()
bool	streq(), imgetb()
double	imgetd()

define	getpar_		99

begin
        call smark (sp)
	call salloc (buf, SZ_LINE, TY_CHAR)
	call salloc (param, SZ_FNAME, TY_CHAR)
	call salloc (emsg, SZ_LINE, TY_CHAR)
	call aclrc (Memc[buf], SZ_LINE)
	call aclrc (Memc[param], SZ_FNAME)
	call aclrc (Memc[emsg], SZ_LINE)

	if (VDEBUG) { call eprintf ("getop: opname=%s  ");call pargstr(opname)}

        # First see if it's one of the special image operands that was 
	# referenced in an "@param" call.

        if (((opname[1] != 'i' && opname[1] != 'b') && !IS_DIGIT(opname[2])) ||
	     (opname[1] == 'i' && opname[2] == '_')) {
	         call strcpy (opname, Memc[param], SZ_FNAME)
                 im = IO_IMPTR(IMOP(ex,1))
getpar_          O_LEN(o) = 0
	         switch (imgftype (im, Memc[param])) {
                 case TY_BOOL:
                     O_TYPE(o) = TY_BOOL
                     O_VALI(o) = btoi (imgetb (im, Memc[param]))
                 case TY_CHAR:
                     O_TYPE(o) = TY_CHAR
                     O_LEN(o)  = SZ_LINE
                     call malloc (O_VALP(o), SZ_LINE, TY_CHAR)
                     call imgstr (im, Memc[param], O_VALC(o), SZ_LINE)
                 case TY_INT:
                     O_TYPE(o) = TY_INT
                     O_VALI(o) = imgeti (im, Memc[param])
                 case TY_REAL:
                     O_TYPE(o) = TY_DOUBLE
                     O_VALD(o) = imgetd (im, Memc[param])
                 default:
                     call sprintf (Memc[emsg], SZ_LINE, "param %s not found\n")
                         call pargstr (Memc[param])
                     call error (6, Memc[emsg])
                 }

	         call sfree (sp)
                 return

        } else if (IS_LOWER(opname[1]) && opname[3] == '.') {
	    # This is a tag.param operand.  Break out the image tag name and
	    # get the image pointer for it, then get the parameter
	    if (opname[1] == 'b') {		# band of 3-D image, only 1 ptr
                imnum = 1
	    } else if (opname[1] == 'i') {	# image descriptor
		i = 2
		if (ctoi (opname, i, imnum) == 0)
                    call error (6, "can't parse operand")
	    } else {
	        call sprintf (Memc[buf], SZ_LINE, 
		    "Unknown outbands operand `%s'\n")
	    	        call pargstr(opname) 
	        call error (1, Memc[buf])
	    }

	    # Get the parameter value.
            im = IO_IMPTR(IMOP(ex,imnum))
	    call strcpy (opname[4], Memc[param], SZ_FNAME)
	    goto getpar_
        }

	nops = EX_NIMOPS(ex)
	found = NO
	do i = 1, nops {
	    # Search for operand name which matches requested value.
	    op = IMOP(ex,i)
            if (streq (Memc[IO_TAG(op)],opname)) {
		found = YES
		break
	    }
	}

	if (VDEBUG && found == YES) {
	    call eprintf (" tag=%s found=%d  ")
		call pargstr(Memc[IO_TAG(op)]) ; call pargi(found)
	    call zze_prop (op)
	}

	if (found == YES) {
	    # Copy operand descriptor to 'o'
	    #optype = ex_ptype (IO_TYPE(op), IO_NBYTES(op))
	    optype = IO_TYPE(op)
	    switch (optype) {
	    case TY_UBYTE, TY_USHORT, TY_SHORT:
		O_LEN(o) = IO_NPIX(op)
		O_TYPE(o) = TY_SHORT
		call malloc (O_VALP(o), IO_NPIX(op), TY_SHORT)
		call amovs (Mems[IO_DATA(op)], Mems[O_VALP(o)], IO_NPIX(op))

	    case TY_INT:
		O_LEN(o) = IO_NPIX(op)
		O_TYPE(o) = TY_INT
		call malloc (O_VALP(o), IO_NPIX(op), TY_INT)
		call amovi (Memi[IO_DATA(op)], Memi[O_VALP(o)], IO_NPIX(op))

	    case TY_LONG:
		O_LEN(o) = IO_NPIX(op)
		O_TYPE(o) = TY_LONG
		call malloc (O_VALP(o), IO_NPIX(op), TY_LONG)
		call amovl (Meml[IO_DATA(op)], Meml[O_VALP(o)], IO_NPIX(op))

	    case TY_REAL:
		O_LEN(o) = IO_NPIX(op)
		O_TYPE(o) = TY_REAL
		call malloc (O_VALP(o), IO_NPIX(op), TY_REAL)
		call amovr (Memr[IO_DATA(op)], Memr[O_VALP(o)], IO_NPIX(op))

	    case TY_DOUBLE:
		O_LEN(o) = IO_NPIX(op)
		O_TYPE(o) = TY_DOUBLE
		call malloc (O_VALP(o), IO_NPIX(op), TY_DOUBLE)
		call amovd (Memd[IO_DATA(op)], Memd[O_VALP(o)], IO_NPIX(op))

	    }
	    
	} else {
	    call sprintf (Memc[buf], SZ_LINE, "Unknown outbands operand `%s'\n")
	    	call pargstr(opname) 
	    call error (1, Memc[buf])
	}

	call sfree (sp)
end


# EX_OBFCN -- Called by evvexpr to execute import outbands special functions.

procedure ex_obfcn (ex, fcn, args, nargs, o)

pointer	ex				#i package pointer
char    fcn[ARB]                        #i function to be executed
pointer args[ARB]                       #i argument list
int     nargs                           #i number of arguments
pointer o                               #o operand pointer

pointer	sp, buf
pointer	r, g, b, gray
pointer	scaled, data
int	i, len, v_nargs, func, nbins
short	sz1, sz2, sb1, sb2, zero
real	gamma, bscale, bzero, scale, pix
real	z1, z2

int	strdic()
bool	fp_equalr(), strne()

define	setop_		99

begin
	call smark (sp)
	call salloc (buf, SZ_FNAME, TY_CHAR)

        # Lookup function in dictionary.
        func = strdic (fcn, Memc[buf], SZ_LINE, OB_FUNCTIONS)
        if (func > 0 && strne(fcn,Memc[buf]))
            func = 0

        # Abort if the function is not known.
        if (func <= 0)
            call xev_error1 ("unknown function `%s' called", fcn)

        # Verify the correct number of arguments, negative value means a
        # variable number of args, handle it in the evaluation.
	switch (func) {
	case GRAY, GREY:
	    v_nargs = 3
	case ZSCALE:
	    v_nargs = -1
	case BSCALE:
	    v_nargs = 3
	case GAMMA:
	    v_nargs = -1
	case BLOCK:
	    v_nargs = 3
	}
        if (v_nargs > 0 && nargs != v_nargs)
            call xev_error2 ("function `%s' requires %d arguments",
                fcn, v_nargs)
        else if (v_nargs < 0 && nargs < abs(v_nargs))
            call xev_error2 ("function `%s' requires at least %d arguments",
                fcn, abs(v_nargs))

        if (DEBUG) {
            call eprintf ("obfcn: nargs=%d func=%d\n")
		call pargi (nargs) ; call pargi (func)
            do i = 1, nargs { call eprintf ("\t") ; call zze_pevop (args[i]) }
	    call flush (STDERR)
        }

	# Evaluate the function.
	zero = 0
	switch (func) {
	case GRAY, GREY:
	    # evaluate expression for NTSC grayscale.
	    r = O_VALP(args[1])
	    g = O_VALP(args[2])
	    b = O_VALP(args[3])
	    len = O_LEN(args[1]) - 1
	    O_LEN(o) = len + 1
	    O_TYPE(o) = TY_REAL
	    call malloc (O_VALP(o), len+1, TY_REAL)
	    gray = O_VALP(o)
            switch (O_TYPE(args[1])) {
            case TY_UBYTE, TY_USHORT, TY_SHORT:
	        do i = 0, len {
		    Memr[gray+i] = R_COEFF * Mems[r+i] +
			           G_COEFF * Mems[g+i] +
			           B_COEFF * Mems[b+i]
	        }

            case TY_INT:
	        do i = 0, len {
		    Memr[gray+i] = R_COEFF * Memi[r+i] +
			           G_COEFF * Memi[g+i] +
			           B_COEFF * Memi[b+i]
	        }

            case TY_LONG:
	        do i = 0, len {
		    Memr[gray+i] = R_COEFF * Meml[r+i] +
			           G_COEFF * Meml[g+i] +
			           B_COEFF * Meml[b+i]
	        }

            case TY_REAL:
	        do i = 0, len {
		    Memr[gray+i] = R_COEFF * Memr[r+i] +
			           G_COEFF * Memr[g+i] +
			           B_COEFF * Memr[b+i]
	        }

            case TY_DOUBLE:
	        do i = 0, len {
		    Memr[gray+i] = R_COEFF * Memd[r+i] +
			           G_COEFF * Memd[g+i] +
			           B_COEFF * Memd[b+i]
	        }

            }

	case ZSCALE:
	    data = O_VALP(args[1])
	    switch (O_TYPE(args[2])) {
	    case TY_SHORT: 	z1 = O_VALS(args[2])
	    case TY_INT: 	z1 = O_VALI(args[2])
	    case TY_LONG: 	z1 = O_VALL(args[2])
	    case TY_REAL: 	z1 = O_VALR(args[2])
	    case TY_DOUBLE: 	z1 = O_VALD(args[2])
	    }
	    switch (O_TYPE(args[3])) {
	    case TY_SHORT: 	z2 = O_VALS(args[3])
	    case TY_INT: 	z2 = O_VALI(args[3])
	    case TY_LONG: 	z2 = O_VALL(args[3])
	    case TY_REAL: 	z2 = O_VALR(args[3])
	    case TY_DOUBLE: 	z2 = O_VALD(args[3])
	    }
	    if (nargs < 4)
	        nbins = 256
	    else
	        nbins = O_VALI(args[4])
            len = O_LEN(args[1])
	    O_LEN(o) = len 
	    O_TYPE(o) = O_TYPE(args[1])
	    call malloc (O_VALP(o), len, O_TYPE(args[1]))
            scaled = O_VALP(o)
            switch (O_TYPE(args[1])) {
            case TY_UBYTE, TY_USHORT, TY_SHORT:
		sz1 = z1
		sz2 = z2
		sb1 = 0
		sb2 = nbins - 1
		if (abs(sz2-sz1) > 1.0e-5)
		    call amaps (Mems[data], Mems[scaled], len, sz1, sz2, 
			sb1, sb2)
		else
		    call amovks (0, Mems[scaled], len)

            case TY_INT:
		if (abs(z2-z1) > 1.0e-5)
		    call amapi (Memi[data], Memi[scaled], len, int (z1), 
		        int(z2), int (0), int (nbins-1))
		else
		    call amovki (int (0), Memi[scaled], len)

            case TY_LONG:
		if (abs(z2-z1) > 1.0e-5)
		    call amapl (Meml[data], Meml[scaled], len, long (z1), 
		        long(z2), long (0), long (nbins-1))
		else
		    call amovkl (long (0), Meml[scaled], len)

            case TY_REAL:
		if (abs(z2-z1) > 1.0e-5)
		    call amapr (Memr[data], Memr[scaled], len, real (z1), 
		        real(z2), real (0), real (nbins-1))
		else
		    call amovkr (real (0), Memr[scaled], len)

            case TY_DOUBLE:
		if (abs(z2-z1) > 1.0e-5)
		    call amapd (Memd[data], Memd[scaled], len, double (z1), 
		        double(z2), double (0), double (nbins-1))
		else
		    call amovkd (double (0), Memd[scaled], len)

            }

	case BSCALE:
	    data = O_VALP(args[1])
	    bzero = O_VALR(args[2])
	    bscale = O_VALR(args[3])
            len = O_LEN(args[1]) - 1
	    O_LEN(o) = len + 1
	    O_TYPE(o) = TY_REAL
	    call malloc (O_VALP(o), len+1, TY_REAL)
            scaled = O_VALP(o)
            switch (O_TYPE(args[1])) {
            case TY_UBYTE, TY_USHORT, TY_SHORT:
		if (!fp_equalr (0.0, bscale)) {
                    do i = 0, len
                        Memr[scaled+i] = (Mems[data+i] - bzero) / bscale
		} else
		    call amovks (zero, Mems[scaled], len)

            case TY_INT:
		if (!fp_equalr (0.0, bscale)) {
                    do i = 0, len
                        Memr[scaled+i] = (Memi[data+i] - bzero) / bscale
		} else
		    call amovki (int(0), Memi[scaled], len)

            case TY_LONG:
		if (!fp_equalr (0.0, bscale)) {
                    do i = 0, len
                        Memr[scaled+i] = (Meml[data+i] - bzero) / bscale
		} else
		    call amovkl (long(0), Meml[scaled], len)

            case TY_REAL:
		if (!fp_equalr (0.0, bscale)) {
                    do i = 0, len
                        Memr[scaled+i] = (Memr[data+i] - bzero) / bscale
		} else
		    call amovkr (real(0), Memr[scaled], len)

            case TY_DOUBLE:
		if (!fp_equalr (0.0, bscale)) {
                    do i = 0, len
                        Memr[scaled+i] = (Memd[data+i] - bzero) / bscale
		} else
		    call amovkd (double(0), Memd[scaled], len)

            }

	case GAMMA:
	    data = O_VALP(args[1])
	    gamma = 1.0 / O_VALR(args[2])
	    if (nargs == 3)
		scale = max (1.0, O_VALR(args[3]))
	    else
		scale = 255.0
            len = O_LEN(args[1]) - 1
	    O_LEN(o) = len + 1
	    O_TYPE(o) = TY_REAL
	    call malloc (O_VALP(o), len+1, TY_REAL)
            scaled = O_VALP(o)
            switch (O_TYPE(args[1])) {
            case TY_UBYTE, TY_USHORT, TY_SHORT:
                do i = 0, len {
		    pix = max (zero, Mems[data+i])
                    Memr[scaled+i] = scale * ((pix/scale) ** gamma)
		}

            case TY_INT:
                do i = 0, len {
		    pix = max (int(0), Memi[data+i])
                    Memr[scaled+i] = scale * ((pix/scale) ** gamma)
		}

            case TY_LONG:
                do i = 0, len {
		    pix = max (long(0), Meml[data+i])
                    Memr[scaled+i] = scale * ((pix/scale) ** gamma)
		}

            case TY_REAL:
                do i = 0, len {
		    pix = max (real(0), Memr[data+i])
                    Memr[scaled+i] = scale * ((pix/scale) ** gamma)
		}

            case TY_DOUBLE:
                do i = 0, len {
		    pix = max (double(0), Memd[data+i])
                    Memr[scaled+i] = scale * ((pix/scale) ** gamma)
		}

            }

        case BLOCK:
            len = O_VALI(args[2])
	    O_LEN(o) = len
	    O_TYPE(o) = O_TYPE(args[1])
	    call malloc (O_VALP(o), len, O_TYPE(args[1]))
            scaled = O_VALP(o)
            switch (O_TYPE(args[1])) {
            case TY_UBYTE, TY_USHORT, TY_SHORT:
		call amovks (O_VALS(args[1]), Mems[scaled], len)
            case TY_INT:
		call amovki (O_VALI(args[1]), Memi[scaled], len)
            case TY_LONG:
		call amovkl (O_VALL(args[1]), Meml[scaled], len)
            case TY_REAL:
		call amovkr (O_VALR(args[1]), Memr[scaled], len)
            case TY_DOUBLE:
		call amovkd (O_VALD(args[1]), Memd[scaled], len)
            }


	}

        if (DEBUG) { call zze_pevop (o) }

	call sfree (sp)
end
