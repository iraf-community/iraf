include	<ctotok.h>
include	<evvexpr.h>
include	"ace.h"

define	COLORS	"|black|white|red|green|blue|yellow|cyan|magenta|transparent|"
define	DEFCOLOR	 203


# MASKCOLOR_MAP -- Create the mask colormap object.

pointer procedure maskcolor_map (colorstring)

char	colorstring		#I Color specification string
pointer	colors			#O Mask colormap object

int	i, j, ip, ncolors, token, lasttoken, maskval1, maskval2, color, offset
int	strdic(), ctoi(), nowhite()
pointer	sp, str, op

int	coltrans[9]
data	coltrans/202,203,204,205,206,207,208,209,-1/

define	err_	10

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# If the colorstring is an expression just save the string
	# and set the number of colors to 0.
	i = nowhite (colorstring, Memc[str], SZ_LINE)
	if (Memc[str] == '(') {
	    call malloc (colors, SZ_LINE, TY_INT)
	    call malloc (op, LEN_OPERAND, TY_STRUCT)
	    Memi[colors] = 0
	    Memi[colors+1] = op
	    call strcpy (colorstring, Memc[P2C(colors+2)], SZ_LINE)
	    O_TYPE(op) = TY_INT
	    O_VALP(op) = NULL
	    O_FLAGS(op) = O_FREEOP
	    # Check expression here.
	    return (colors)
	}

	# Allocate memory for the colormap object.
	call malloc (colors, 4*10, TY_INT)

	# Initialize
	ncolors = 1
	maskval1 = INDEFI
	maskval2 = INDEFI
	color = DEFCOLOR
	offset = NO

	Memi[colors] = ncolors
	Memi[colors+2] = color
	Memi[colors+3] = offset

	# Parse the color specification.
	token = 0
	call sscan (colorstring)
	repeat {
	    lasttoken = token
	    call gargtok (token, Memc[str], SZ_LINE)
	    switch (token) {
	    case TOK_IDENTIFIER:
		call strlwr (Memc[str])
		i = strdic (Memc[str], Memc[str], SZ_LINE, COLORS)
		if (i == 0)
		    goto err_
		color = coltrans[i]
	    case TOK_NUMBER:
		if (lasttoken == TOK_NUMBER) {
		    if (Memc[str] != '-')
			goto err_
		    ip = 2
		    if (ctoi (Memc[str], ip, maskval2) == 0)
			goto err_
		} else {
		    if (Memc[str] == '+') {
			offset = YES
			ip = 2
		    } else if (Memc[str] == '-') {
			offset = YES
			ip = 1
		    } else
			ip = 1
		    if (ctoi (Memc[str], ip, color) == 0)
			goto err_
		    if (lasttoken != TOK_OPERATOR)
			maskval2 = color
		}
	    case TOK_OPERATOR:
		if (Memc[str] != '=' || lasttoken != TOK_NUMBER)
		    goto err_
		maskval1 = min (color, maskval2)
		maskval2 = max (color, maskval2)

		if (Memc[str+1] == '+') {
		    call gargtok (token, Memc[str+2], SZ_LINE)
		    offset = YES
		    ip = 3
		    if (ctoi (Memc[str], ip, color) == 0)
			goto err_
		} else if (Memc[str+1] == '-') {
		    call gargtok (token, Memc[str+2], SZ_LINE)
		    offset = YES
		    ip = 2
		    if (ctoi (Memc[str], ip, color) == 0)
			goto err_
		}
	    case TOK_PUNCTUATION, TOK_EOS:
		if (Memc[str] != ',' && Memc[str] != EOS)
		    goto err_
		if (!IS_INDEFI(maskval1)) {
		    do i = 2, ncolors {
			j = 4 * i - 4
			if (Memi[colors+j] == maskval1 &&
			    Memi[colors+j+1] == maskval2)
			    break
		    }
		    if (i > ncolors) {
			if (mod (ncolors, 10) == 0)
			    call realloc (colors, 4*(ncolors+10), TY_INT)
			ncolors = ncolors + 1
		    }
		    j = 4 * i - 4
		    Memi[colors+j] = maskval1
		    Memi[colors+j+1] = maskval2
		    Memi[colors+j+2] = color
		    Memi[colors+j+3] = offset
		} else {
		    Memi[colors+2] = color
		    Memi[colors+3] = offset
		}
		if (token == TOK_EOS)
		    break
		maskval1 = INDEFI
		maskval2 = INDEFI
		offset = NO
	    default:
		goto err_
	    }
	}

	Memi[colors] = ncolors
	call sfree (sp)
	return (colors)

err_
	call mfree (colors, TY_INT)
	call sfree (sp)
	call error (1, "Error in color specifications")
end


# MASKCOLOR_FREE -- Free the mask color object.

procedure maskcolor_free (colors)

pointer	colors			#I Mask colormap object

begin
	if (Memi[colors] == 0)
	    call evvfree (Memi[colors+1])
	call mfree (colors, TY_INT)
end


# MASKCOLOR -- Return a color for a mask value.

int procedure maskcolor (colors, maskval)

pointer	colors			#I Mask colormap object
int	maskval			#I Mask value
int	color			#O Color value

int	i, j, offset

begin
	# If there is no color array return the mask value.
	if (Memi[colors] == 0)
	    return (maskval)

	color = Memi[colors+2]
	offset = Memi[colors+3]
	do i = 2, Memi[colors] {
	    j = 4 * i - 4
	    if (maskval >= Memi[colors+j] && maskval <= Memi[colors+j+1]) {
		color = Memi[colors+j+2]
		offset = Memi[colors+j+3]
		break
	    }
	}

	if (offset == YES)
	    color = maskval + color
	return (color)
end


procedure maskexprn (colors, maskvals, nmaskvals)

pointer	colors			#I Mask colormap object
pointer	maskvals		#O Pointer to mask values (TY_INT)
int	nmaskvals		#I Number of mask values

int	i
pointer	op, o, evvexpr()
errchk	evvexpr

int	locpr
extern	maskoperand, maskfunc

begin
	if (Memi[colors] > 0)
	    return

	op = Memi[colors+1]
	O_LEN(op) = nmaskvals
	O_VALP(op) = maskvals

	o = evvexpr (Memc[P2C(colors+2)], locpr(maskoperand), op,
	    locpr(maskfunc), NULL, O_FREEOP)

	#call amovi (Memi[O_VALP(o)], Memi[maskvals], nmaskvals)
	switch (O_TYPE(o)) {
	case TY_SHORT:
	    do i = 0, O_LEN(o) {
	        if (Memi[maskvals+i] > 0)
		    Memi[maskvals+i] = max (0, Mems[O_VALP(o)+i])
	    }
	case TY_BOOL, TY_INT:
	    do i = 0, O_LEN(o) {
	        if (Memi[maskvals+i] > 0)
		    Memi[maskvals+i] = max (0, Memi[O_VALP(o)+i])
	    }
	case TY_REAL:
	    do i = 0, O_LEN(o) {
	        if (Memi[maskvals+i] > 0)
		    Memi[maskvals+i] = max (0, nint(Memr[O_VALP(o)+i]))
	    }
	case TY_DOUBLE:
	    do i = 0, O_LEN(o) {
	        if (Memi[maskvals+i] > 0)
		    Memi[maskvals+i] = max (0, nint(Memd[O_VALP(o)+i]))
	    }
	}
	    
	call evvfree (o)
end


# MASKOPERAND -- Handle mask expression operands.

procedure maskoperand (op, operand, o)

pointer	op			#I Input operand pointer
char	operand[ARB]		#I Operand name
pointer	o			#O Operand object

char	str[10]
int	i, coltrans[9], strdic()
data	coltrans/202,203,204,205,206,207,208,209,-1/

begin
	if (operand[1] == '$') {
	    call xvv_initop (o, O_LEN(op), O_TYPE(op))
	    call amovi (Memi[O_VALP(op)], Memi[O_VALP(o)], O_LEN(op))
	    return
	}

	call strcpy (operand, str, 11)
	call strlwr (str)
	i = strdic (str, str, 11, COLORS)
	if (i > 0) {
	    call xvv_initop (o, 0, TY_INT)
	    O_VALI(o) = coltrans[i]
	    return
	}

	call xvv_error1 ("Unknown mask operand %s", operand)
end


define	KEYWORDS "|acenum|colors|"

define	F_ACENUM	1	# acenum (maskcodes,[flags])
define	F_COLORS	2	# colors (maskcodes,[col1,col2,col3])

# MASKFUNC -- Special processing functions.

procedure maskfunc (data, func, args, nargs, val)

pointer	data			#I client data
char	func[ARB]		#I function to be called
pointer	args[ARB]		#I pointer to arglist descriptor
int	nargs			#I number of arguments
pointer	val			#O output operand (function value)

char	str[12]
int	i, j, c1, c2, c3
int	iresult, optype, oplen, opcode, v_nargs
double	dresult

bool	strne()
int	strdic(), btoi(), andi()
errchk	malloc

begin
	# Lookup the function name in the dictionary.  An exact match is
	# required (strdic permits abbreviations).  Abort if the function
	# is not known.

	opcode = strdic (func, str, 12, KEYWORDS)
	if (strne (func, str))
	    call xvv_error1 ("unknown function `%s' called", func)

	# Verify correct number of arguments.
	switch (opcode) {
	case F_ACENUM, F_COLORS:
	    v_nargs = -1
	default:
	    v_nargs = 1
	}

	if (v_nargs > 0 && nargs != v_nargs)
	    call xvv_error2 ("function `%s' requires %d arguments",
		func, v_nargs)
	else if (v_nargs < 0 && nargs < abs(v_nargs))
	    call xvv_error2 ("function `%s' requires at least %d arguments",
		func, abs(v_nargs))

	# Group some common operations.
	switch (opcode) {
	case F_ACENUM:
	    # Check types of arguments.
	    if (O_TYPE(args[1]) != TY_INT)
		call xvv_error1 ("error in argument types for function `%s'",
		    func)
	    if (nargs > 1) {
		if (O_TYPE(args[2]) != TY_CHAR)
		    call xvv_error1 (
		        "error in argument types for function `%s'", func)
	    }
	    optype = TY_INT
	    oplen = O_LEN(args[1])
	    if (oplen > 0)
		call malloc (iresult, oplen, TY_INT)
	case F_COLORS:
	    # Check types of arguments.
	    do i = 1, nargs {
		if (O_TYPE(args[i]) != TY_INT)
		    call xvv_error1 ("function `%s' requires integer arguments",
			func)
	    }
	    optype = TY_INT
	    oplen = O_LEN(args[1])
	    if (oplen > 0)
		call malloc (iresult, oplen, TY_INT)
	}

	# Evaluate the function.
	switch (opcode) {
	case F_ACENUM:
	    if (nargs == 1)
	       call strcpy ("BDEG", str, 12)
	    else
	       call strcpy (O_VALC(args[2]), str, 12)
	    call strupr (str)
	    c1 = 0; c2 = 0
	    for (i=1; str[i]!=EOS; i=i+1) {
		switch (str[i]) {
		case 'B':
		    c1 = c1 + MASK_BP
		case 'D':
		    c2 = c2 + MASK_GRW + MASK_SPLIT
		case 'E':
		    c1 = c1 + MASK_BNDRY
		case 'F':
		    c1 = c1 + MASK_BPFLAG
		case 'G':
		    c1 = c1 + MASK_GRW
		case 'S':
		    c1 = c1 + MASK_SPLIT
		}
	    }

	    if (oplen == 0) {
		i = O_VALI(args[1])
		if (i > 10) {
		    if (andi(i,c1)!=0 && andi(i,c2)==0)
		        i = MNUM(i)
		    else
		        i = -1
		} else
		    i = 0
		iresult = i
	    } else {
	        do j = 0, oplen-1 {
		    i = Memi[O_VALP(args[1])+j]
		    if (i > 10) {
			if (andi(i,c1)!=0)
			    i = MNUM(i)
			else if (c2 != 0 && i <= MASK_NUM)
			    i = MNUM(i)
			else
			    i = -1
		    } else
		        i = 0
		    Memi[iresult+j] = i
		}
	    }
	case F_COLORS:
	    c1 = 0; c2 = 204; c3 = 217
	    if (nargs > 1)
	        c1 = O_VALI(args[2])
	    if (nargs > 2) {
	        c2 = O_VALI(args[3])
		c3 = c2
	    }
	    if (nargs > 3)
	        c3 = O_VALI(args[4])
	    if (c3 < c2) {
	        i = c2; c2 = c3; c3 = i
	    }
	    c3 = c3 - c2 + 1

	    optype = TY_INT
	    oplen = O_LEN(args[1])
	    if (oplen == 0) {
		i = O_VALI(args[1])
		if (i == 0)
		    i = c1
		else if (i > 0)
		    i = c2 + mod (i-1, c3)
		iresult = i
	    } else {
	        do j = 0, oplen-1 {
		    i = Memi[O_VALP(args[1])+j]
		    if (i == 0)
			i = c1
		    else if (i > 0)
			i = c2 + mod (i-1, c3)
		    Memi[iresult+j] = i
		}
	    }
	}

	# Write the result to the output operand.  Bool results are stored in
	# iresult as an integer value, string results are stored in iresult as
	# a pointer to the output string, and integer and real/double results
	# are stored in iresult and dresult without any tricks.

	call xvv_initop (val, oplen, optype)
	if (oplen == 0) {
	    switch (optype) {
	    case TY_BOOL:
		O_VALI(val) = btoi (iresult != 0)
	    case TY_CHAR:
		O_VALP(val) = iresult
	    case TY_INT:
		O_VALI(val) = iresult
	    case TY_REAL:
		O_VALR(val) = dresult
	    case TY_DOUBLE:
		O_VALD(val) = dresult
	    }
	} else {
	    O_VALP(val) = iresult
	    O_FLAGS(val) = O_FREEVAL
	}

	# Free any storage used by the argument list operands.
	do i = 1, nargs
	    call xvv_freeop (args[i])

end
