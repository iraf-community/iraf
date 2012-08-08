include <error.h>
include <mach.h>
include <evvexpr.h>
include <fset.h>
include "../import.h"
include "../ipfcn.h"

define	DEBUG	false
define	VDEBUG	false


# IP_GETOP -- Called by evvexpr to get an operand.  

procedure ip_getop (ip, opname, o)

pointer	ip					#i task struct pointer
char	opname[ARB]				#i operand name to retrieve
pointer	o					#o output operand pointer

int	i, nops, found, optype
pointer	sp, buf
pointer	op

int	fstati(), ip_ptype(), strlen(), strncmp()
bool	streq()

begin
	# First see if it's one of the special file operands.
	if (opname[1] == '$') {
	    if (strncmp(opname, "$FSIZE", 3) == 0) {
		O_LEN(o) = 0
		O_TYPE(o) = TY_INT
		O_VALI(o) = fstati (IP_FD(ip), F_FILESIZE) * SZB_CHAR
	    } else if (strncmp(opname, "$FNAME", 3) == 0) {
	        call smark (sp)
	        call salloc (buf, SZ_FNAME, TY_CHAR)

		call fstats (IP_FD(ip), F_FILENAME, Memc[buf], SZ_FNAME)

		O_TYPE(o) = TY_CHAR
		O_LEN(o) = strlen (Memc[buf]) + 1
		call malloc (O_VALP(o), O_LEN(o), TY_CHAR)
		call strcpy (Memc[buf], O_VALC(o), i)
		call sfree (sp)
	    }

	    return
	}
	
	nops = IP_NPIXT(ip)
	found = NO
	do i = 1, nops {
	    # Search for operand name which matches requested value.
	    op = PTYPE(ip,i)
            if (streq (Memc[IO_TAG(op)],opname)) {
		found = YES
		break
	    }
	}

	if (VDEBUG) {
	    call eprintf ("getop: opname=%s tag=%s found=%d  ")
	    	call pargstr(opname) ; call pargstr(Memc[IO_TAG(op)])
		call pargi(found)
	    if (found == YES) call zzi_prop (op)
	}

	if (found == YES) {
	    # Copy operand descriptor to 'o'
	    optype = ip_ptype (IO_TYPE(op), IO_NBYTES(op))
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
	    call smark (sp)
	    call salloc (buf, SZ_LINE, TY_CHAR)
	    call sprintf (Memc[buf], SZ_LINE, "Unknown outbands operand `%s'\n")
	    	call pargstr(opname) 
	    call sfree (sp)
	    call error (1, Memc[buf])
	}
end


# IP_EVALUATE -- Evaluate the outbands expression.

pointer procedure ip_evaluate (ip, expr)

pointer	ip					#i task struct pointer
char	expr[ARB]				#i expression to be evaluated

pointer	o					# operand pointer to result

int     locpr()
pointer evvexpr()
extern  ip_getop(), ip_obfcn()
errchk  evvexpr

begin
	if (DEBUG) { call eprintf("ip_eval: expr='%s'\n") ; call pargstr(expr) }

        # Evaluate the expression.
        iferr {
            o = evvexpr (expr, locpr(ip_getop), ip, locpr(ip_obfcn), ip, 
		EV_RNGCHK)
         } then
             call erract (EA_FATAL)

	return (o)
end


# IP_OBFCN -- Called by evvexpr to execute import outbands special functions.

procedure ip_obfcn (ip, fcn, args, nargs, o)

pointer ip                                      #i task struct pointer
char    fcn[ARB]                                #i function to be executed
pointer args[ARB]                               #i argument list
int     nargs                                   #i number of arguments
pointer o                                       #o operand pointer

pointer	sp, buf
pointer	r, g, b, gray, color, cmap
int	i, len, v_nargs, func

int	or(), strdic()
bool	strne()

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
	case FLIPX, FLIPY:
	    v_nargs = 1
	case RED, GREEN, BLUE:
	    v_nargs = 1
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
            do i = 1, nargs { call eprintf ("\t") ; call zzi_pevop (args[i]) }
	    call flush (STDERR)
        }

	# Evaluate the function.
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

	case RED:
	    # Get the red colormap component of the image.
	    cmap = IP_CMAP(ip)
            if (func <= 0)
                call xev_error1 ("No colormap in image for function `%s'", fcn)
	    r = O_VALP(args[1])
	    len = O_LEN(args[1]) - 1
            O_LEN(o) = len + 1
            O_TYPE(o) = TY_SHORT
            call malloc (O_VALP(o), len+1, TY_SHORT)
	    color = O_VALP(o)
            switch (O_TYPE(args[1])) {
            case TY_UBYTE, TY_USHORT, TY_SHORT:
	        do i = 0, len
		    Mems[color+i] = CMAP(cmap,IP_RED,Mems[r+i]+1)

            case TY_INT:
	        do i = 0, len 
		    Mems[color+i] = CMAP(cmap,IP_RED,Memi[r+i]+1) 

            case TY_LONG:
	        do i = 0, len 
		    Mems[color+i] = CMAP(cmap,IP_RED,Meml[r+i]+1) 

            }

	case GREEN:
	    # Get the blue colormap component of the image.
	    cmap = IP_CMAP(ip)
            if (func <= 0)
                call xev_error1 ("No colormap in image for function `%s'", fcn)
	    g = O_VALP(args[1])
	    len = O_LEN(args[1]) - 1
            O_LEN(o) = len + 1
            O_TYPE(o) = TY_SHORT
            call malloc (O_VALP(o), len+1, TY_SHORT)
	    color = O_VALP(o)
            switch (O_TYPE(args[1])) {
            case TY_UBYTE, TY_USHORT, TY_SHORT:
	        do i = 0, len
		    Mems[color+i] = CMAP(cmap,IP_GREEN,Mems[g+i]+1)

            case TY_INT:
	        do i = 0, len
		    Mems[color+i] = CMAP(cmap,IP_GREEN,char(Memi[g+i]+1))

            case TY_LONG:
	        do i = 0, len
		    Mems[color+i] = CMAP(cmap,IP_GREEN,char(Meml[g+i]+1))

            }

	case BLUE:
	    # Get the blue colormap component of the image.
	    cmap = IP_CMAP(ip)
            if (func <= 0)
                call xev_error1 ("No colormap in image for function `%s'", fcn)
	    b = O_VALP(args[1])
	    len = O_LEN(args[1]) - 1
            O_LEN(o) = len + 1
            O_TYPE(o) = TY_SHORT
            call malloc (O_VALP(o), len+1, TY_SHORT)
	    color = O_VALP(o)
            switch (O_TYPE(args[1])) {
            case TY_UBYTE, TY_USHORT, TY_SHORT:
	        do i = 0, len
		    Mems[color+i] = CMAP(cmap,IP_BLUE,Mems[b+i]+1)

            case TY_INT:
	        do i = 0, len
		    Mems[color+i] = CMAP(cmap,IP_BLUE,char(Memi[b+i]+1))

            case TY_LONG:
	        do i = 0, len
		    Mems[color+i] = CMAP(cmap,IP_BLUE,char(Meml[b+i]+1))

            }

	case FLIPX:
	    # Set flag to reverse pixel order on output.
	    IP_FLIP(ip) = or (IP_FLIP(ip), FLIP_X)
	    goto setop_ 

	case FLIPY:
	    # Set flag to write image from bottom to top.
	    IP_FLIP(ip) = or (IP_FLIP(ip), FLIP_Y)

            # Copy argument operand descriptor to 'o'
setop_      switch (O_TYPE(args[1])) {
            case TY_UBYTE, TY_USHORT, TY_SHORT:
                O_LEN(o) = O_LEN(args[1])
                O_TYPE(o) = TY_SHORT
                call malloc (O_VALP(o), O_LEN(args[1]), TY_SHORT)
                call amovs (Mems[O_VALP(ARGS[1])], Mems[O_VALP(o)], O_LEN(o))

            case TY_INT:
                O_LEN(o) = O_LEN(args[1])
                O_TYPE(o) = TY_INT
                call malloc (O_VALP(o), O_LEN(args[1]), TY_INT)
                call amovi (Memi[O_VALP(args[1])], Memi[O_VALP(o)], O_LEN(o))

            case TY_LONG:
                O_LEN(o) = O_LEN(args[1])
                O_TYPE(o) = TY_LONG
                call malloc (O_VALP(o), O_LEN(args[1]), TY_LONG)
                call amovl (Meml[O_VALP(args[1])], Meml[O_VALP(o)], O_LEN(o))

            case TY_REAL:
                O_LEN(o) = O_LEN(args[1])
                O_TYPE(o) = TY_REAL
                call malloc (O_VALP(o), O_LEN(args[1]), TY_REAL)
                call amovr (Memr[O_VALP(args[1])], Memr[O_VALP(o)], O_LEN(o))

            case TY_DOUBLE:
                O_LEN(o) = O_LEN(args[1])
                O_TYPE(o) = TY_DOUBLE
                call malloc (O_VALP(o), O_LEN(args[1]), TY_DOUBLE)
                call amovd (Memd[O_VALP(args[1])], Memd[O_VALP(o)], O_LEN(o))

            }

	}

        if (DEBUG) { call zzi_pevop (o) }

	call sfree (sp)
end
