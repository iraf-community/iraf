include <mach.h>
include <ctype.h>
include <imhdr.h>
include <imset.h>
include <pmset.h>
include <evvexpr.h>

define	DEF_LINELEN	8192

define	LEN_RGEXPR	25
define	RG_PMIM		Memi[$1]		# the mask image
define	RG_PMIBUF	Memi[$1+1]		# the mask input data
define	RG_IPMV		Meml[P2L($1+2+($2)-1)]	# input position in mask image
define	RG_OPMV		Meml[P2L($1+9+($2)-1)]	# output position in mask image


# ME_RGMASK -- Given a region expression, a condition equals true expression,
# a condition equals false expression, and an existing pixel mask imio
# descriptor of dimensions, size of each dimension, and depth in bits create
# a mask image and return an imio pointer to the mask.

int procedure me_rgmask (rexpr, texpr, fexpr, pmim)

char	rexpr[ARB]		#I the boolean region expression
char	texpr[ARB]		#I the condition equals true expression
char	fexpr[ARB]		#I the condition equals true expression
pointer	pmim			#I the pixel mask imio descriptor

pointer	sp, rg, oexpr, expr, obuf
int	i, npix, nlines, depth, pmaxval, stat

pointer	evvexpr()
int	imstati(), locpr(), pm_stati()
int	imgnli(), impnli(), impnls(), impnll()
extern	rg_getop(), rg_fcn()

begin
	# Allocate some work space.
	call smark (sp)
	call salloc (expr, 3 * SZ_LINE, TY_CHAR)

	# Allocate space for the mask expression structure.
	call calloc (rg, LEN_RGEXPR, TY_STRUCT)
	RG_PMIM(rg) = pmim

	# Initalize the i/o pointers.
	call amovkl (long(1), RG_OPMV(rg,1), IM_MAXDIM)
	call amovkl (long(1), RG_IPMV(rg,1), IM_MAXDIM)

	# Create the conditional expression to be evaluated.
	call sprintf (Memc[expr], 3 * SZ_LINE, "(%s) ? %s : %s")
	    call pargstr (rexpr)
	    call pargstr (texpr)
	    call pargstr (fexpr)

	# Compute the total number of output image lines.
	npix = IM_LEN(pmim,1)
	nlines = 1
	do i = 2, IM_NDIM(pmim)
	    nlines = nlines * IM_LEN(pmim, i)
	depth = INDEFI

	# Loop over the mask output image lines which are by default always
	# integer.
	stat = OK
	do i = 1, nlines {

	    # Get the input mask lines.
	    if (imgnli (pmim, RG_PMIBUF(rg), RG_IPMV(rg,1)) == EOF)
		call error (2, "Error reading input mask data")

	    # Determine the depth of the mask.
	    if (IS_INDEFI(depth)) {
	        depth = pm_stati (imstati (pmim, IM_PLDES), P_DEPTH)
		if (depth > 0) {
		    pmaxval = min (depth, PL_MAXDEPTH)
		    pmaxval = 2 ** depth - 1
		} else
		    pmaxval = 2 ** PL_MAXDEPTH - 1
	    }

	    # Evalute the expression.
	    oexpr = evvexpr (Memc[expr], locpr(rg_getop), rg, locpr(rg_fcn),
	        rg, 0)
	    if (O_TYPE(oexpr) == ERR) {
		call eprintf ("Error evaluting expression\n")
		stat = ERR
		break
	    }

	    # Copy the evaluated expression to the image.
	    if (O_LEN(oexpr) == 0) {
		switch (O_TYPE(oexpr)) {
		case TY_BOOL:
		    if (impnli (pmim, obuf, RG_OPMV(rg,1)) == EOF)
			call error (2, "Error writing output mask data")
		    call pl_pixropi (NULL, 1, MAX_INT, Memi[obuf], 1, pmaxval,
		        npix, PIX_CLR + PIX_VALUE(O_VALI(oexpr))) 
		case TY_SHORT:
		    if (impnls (pmim, obuf, RG_OPMV(rg,1)) == EOF)
			call error (2, "Error writing output mask data")
		    call pl_pixrops (NULL, 1, MAX_SHORT, Mems[obuf], 1,
		        pmaxval, npix, PIX_CLR + PIX_VALUE(O_VALS(oexpr))) 
		case TY_INT:
		    if (impnli (pmim, obuf, RG_OPMV(rg,1)) == EOF)
			call error (2, "Error writing output mask data")
		    call pl_pixropi (NULL, 1, MAX_INT, Memi[obuf], 1,
		        pmaxval, npix, PIX_CLR + PIX_VALUE(O_VALI(oexpr))) 
		case TY_LONG:
		    if (impnll (pmim, obuf, RG_OPMV(rg,1)) == EOF)
			call error (2, "Error writing output mask data")
		    call pl_pixropl (NULL, 1, MAX_LONG, Meml[obuf], 1,
		        pmaxval, npix, PIX_CLR + PIX_VALUE(O_VALL(oexpr))) 
		case TY_REAL:
		    call error (3, "Type real expressions are not supported")
		case TY_DOUBLE:
		    call error (3, "Type double expressions are not supported")
		default:
		    call error (3, "Unknown expression value type")
		}

	    } else {
		switch (O_TYPE(oexpr)) {
		case TY_BOOL:
		    if (impnli (pmim, obuf, RG_OPMV(rg,1)) == EOF)
			call error (2, "Error writing output mask data")
		    call pl_pixropi (Memi[O_VALP(oexpr)], 1, MAX_INT,
		        Memi[obuf], 1, pmaxval, npix, PIX_SRC) 
		case TY_SHORT:
		    if (impnls (pmim, obuf, RG_OPMV(rg,1)) == EOF)
			call error (2, "Error writing output mask data")
		    call pl_pixrops (Mems[O_VALP(oexpr)], 1, MAX_SHORT,
		        Mems[obuf], 1, pmaxval, npix, PIX_SRC) 
		case TY_INT:
		    if (impnli (pmim, obuf, RG_OPMV(rg,1)) == EOF)
			call error (2, "Error writing output mask data")
		    call pl_pixropi (Memi[O_VALP(oexpr)], 1, MAX_INT,
		        Memi[obuf], 1, pmaxval, npix, PIX_SRC) 
		case TY_LONG:
		    if (impnll (pmim, obuf, RG_OPMV(rg,1)) == EOF)
			call error (2, "Error writing output mask data")
		    call pl_pixropl (Meml[O_VALP(oexpr)], 1, MAX_LONG,
		        Meml[obuf], 1, pmaxval, npix, PIX_SRC) 
		case TY_REAL:
		    call error (3, "Type real expressions are not supported")
		case TY_DOUBLE:
		    call error (3, "Type double expressions are not supported")
		default:
		    call error (3, "Unknown expression value type")
		}
	    }

	    call evvfree (oexpr)
	}

	# Cleanup.
	call mfree (rg, TY_STRUCT)

	call sfree (sp)

	return (stat)
end


# RG_GETOP -- Called by evvexpr to fetch an input image operand.

procedure rg_getop (rg, opname, o)

pointer	rg			#I mskexpr descriptor
char	opname[ARB]		#I operand name
pointer	o			#I output operand to be filled in

pointer	sp, param, data, im
int	i, axis
int	imgftype(), btoi()
double	imgetd()
int	imgeti()
bool	imgetb()
errchk	malloc
define	err_ 91

begin
	call smark (sp)

	# Pixel image operand.
	if ((opname[1] == 'p') && (opname[2] == EOS)) {

	    if (RG_PMIM(rg) == NULL)
		goto err_

	    O_TYPE(o) = TY_INT
	    O_LEN(o) = IM_LEN(RG_PMIM(rg), 1)
	    O_FLAGS(o) = 0
	    O_VALP(o) = RG_PMIBUF(rg)

	    call sfree (sp)
	    return

	# Reference image header parameter operand.
	} else if ((opname[1] == 'p') && (opname[2] == '.')) {

	    im = RG_PMIM(rg) 
	    if (im == NULL)
		goto err_

	    # Get the parameter value and set up operand struct.
	    call salloc (param, SZ_FNAME, TY_CHAR)
	    call strcpy (opname[3], Memc[param], SZ_FNAME)
	    iferr (O_TYPE(o) = imgftype (im, Memc[param]))
		goto err_

	    switch (O_TYPE(o)) {

	    case TY_BOOL:
		O_LEN(o) = 0
		iferr (O_VALI(o) = btoi (imgetb (im, Memc[param])))
		    goto err_

	    case TY_CHAR:
		O_LEN(o) = SZ_LINE
		O_FLAGS(o) = O_FREEVAL
		iferr {
		    call malloc (O_VALP(o), SZ_LINE, TY_CHAR)
		    call imgstr (im, Memc[param], O_VALC(o), SZ_LINE)
		} then
		    goto err_

	    case TY_SHORT, TY_INT, TY_LONG:
		iferr (O_VALI(o) = imgeti (im, Memc[param]))
		    goto err_

	    case TY_REAL, TY_DOUBLE:
		O_TYPE(o) = TY_DOUBLE
		iferr (O_VALD(o) = imgetd (im, Memc[param]))
		    goto err_

	    default:
		goto err_
	    }

	    call sfree (sp)
	    return

	# The current pixel coordinate [I,J,K,...].  The line coordinate
	# is a special case since the image is computed a line at a time.
	# If "I" is requested return a vector where v[i] = i.  For J, K,
	# etc. just return the scalar index value.

	} else if (IS_UPPER(opname[1]) && opname[2] == EOS) {

	    axis = opname[1] - 'I' + 1
	    if (axis == 1) {
		O_TYPE(o) = TY_INT
		if (IM_LEN(RG_PMIM(rg), 1) > 0)
		    O_LEN(o) = IM_LEN(RG_PMIM(rg), 1)
		else 
		    O_LEN(o) = DEF_LINELEN
		call malloc (data, O_LEN(o), TY_INT)
		do i = 1, O_LEN(o)
		    Memi[data+i-1] = i
		O_VALP(o) = data
		O_FLAGS(o) = O_FREEVAL
	    } else {
		O_TYPE(o) = TY_INT
		if (IM_LEN(RG_PMIM(rg), 1) > 0)
		    O_LEN(o) = IM_LEN(RG_PMIM(rg), 1)
		else 
		    O_LEN(o) = DEF_LINELEN
		call malloc (data, O_LEN(o), TY_INT)
		if (axis < 1 || axis > IM_MAXDIM)
		    call amovki (1, Memi[data], O_LEN(o))
		else
		    call amovki (RG_OPMV(rg,axis), Memi[data], O_LEN(o))
		O_VALP(o) = data
		O_FLAGS(o) = O_FREEVAL
	    }

	    call sfree (sp)
	    return
	}

err_
	O_TYPE(o) = ERR
	call sfree (sp)
end


# define the builtin functions

define	RG_FUNCS	"|circle|ellipse|box|rectangle|polygon|cols|lines|\
vector|pie|cannulus|eannulus|rannulus|pannulus|point|"

define	RG_CIRCLE	1
define	RG_ELLIPSE	2
define	RG_BOX		3
define	RG_RECTANGLE	4
define	RG_POLYGON	5
define	RG_COLS		6
define	RG_LINES	7
define	RG_VECTOR	8
define	RG_PIE		9
define	RG_CANNULUS	10
define	RG_EANNULUS	11
define	RG_RANNULUS	12
define	RG_PANNULUS	13
define	RG_POINT	14


# RG_FCN -- Called by evvexpr to execute a mskexpr special function.

procedure rg_fcn (rg, fcn, args, nargs, o)

pointer	rg			#I imexpr descriptor
char	fcn[ARB]		#I function name
pointer	args[ARB]		#I input arguments
int	nargs			#I number of input arguments
pointer	o			#I output operand to be filled in

real	width
pointer	sp, ufunc, rval1, rval2, orval1, orval2, ix, iy
int	i, ip, func, v_nargs, nver
int	strdic(), ctor()
bool	strne()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (ufunc, SZ_LINE, TY_CHAR)

	# Get the function.
	func = strdic (fcn, Memc[ufunc], SZ_LINE, RG_FUNCS)
	if (func > 0 && strne (fcn, Memc[ufunc]))
	    func = 0

	# Test the function.
        if (func <= 0) {
	    O_TYPE(o) = ERR
	    call sfree (sp)
	    return
	}

	# Determine number of arguments. This is a separate case statement.
	# in case we need to deal with a variable number of arguments 
	# function at a later point.
	switch (func) {
	case RG_POINT, RG_CIRCLE, RG_ELLIPSE, RG_BOX, RG_RECTANGLE, RG_POLYGON:
	    v_nargs = -1
	case RG_CANNULUS, RG_EANNULUS, RG_RANNULUS, RG_PANNULUS:
	    v_nargs = -1
	case RG_COLS, RG_LINES:
	    v_nargs = -1
	case RG_VECTOR, RG_PIE:
	    v_nargs = -1
	default:
	    v_nargs = 0
	}

	# Check the number of arguments.
	if (v_nargs > 0 && nargs != v_nargs) {
	    O_TYPE(o) = ERR
	    call sfree (sp)
	    return
	}
	if (v_nargs < 0 && nargs < abs (v_nargs)) {
	    O_TYPE(o) = ERR
	    call sfree (sp)
	    return
	}

	if (func == RG_POLYGON  && nargs < 6) {
	    O_TYPE(o) = ERR
	    call sfree (sp)
	    return
	}

	# Type convert the arguments appropriately. At the moment this is
	# simple if we assume that all the required arguments are real.
	call salloc (rval1, nargs, TY_REAL)
	call salloc (rval2, nargs, TY_REAL)
	do i = 1, nargs {
	    switch (O_TYPE(args[i])) {
	    case TY_CHAR:
                ip = 1
                if (ctor (O_VALC(args[i]), ip, Memr[rval1+i-1]) == 0)
                    Memr[rval1+i-1] = 0.
	    case TY_INT:
		Memr[rval1+i-1] = O_VALI(args[i])
	    case TY_REAL:
		Memr[rval1+i-1] = O_VALR(args[i])
	    case TY_DOUBLE:
		Memr[rval1+i-1] = O_VALD(args[i])
	    }
	}

	# Evaluate the function. Worry about some duplication of code later.
	switch (func) {

	case RG_CIRCLE:
	    O_LEN(o) = IM_LEN(RG_PMIM(rg),1)
	    O_TYPE(o) = TY_BOOL
	    if (nargs == 5) {
	        call malloc (O_VALP(o), O_LEN(o), TY_INT)
	        call me_circle (Memi[O_VALP(args[1])], Memi[O_VALP(args[2])],
	            Memi[O_VALP(o)], O_LEN(o), Memr[rval1+2], Memr[rval1+3],
		    Memr[rval1+4])
	    } else if (nargs == 3) {
	        call malloc (ix, O_LEN(o), TY_INT)
	        call malloc (iy, O_LEN(o), TY_INT)
	        do i = 1, O_LEN(o)
		    Memi[ix+i-1] = i
	        call amovki (RG_OPMV(rg,2), Memi[iy], O_LEN(o))
	        call malloc (O_VALP(o), O_LEN(o), TY_INT)
	        call me_circle (Memi[ix], Memi[iy], Memi[O_VALP(o)], O_LEN(o),
	            Memr[rval1], Memr[rval1+1], Memr[rval1+2])
	        call mfree (ix, TY_INT)
	        call mfree (iy, TY_INT)
	    } else {
	        O_TYPE(o) = ERR
	    }

	case RG_ELLIPSE:
	    O_LEN(o) = IM_LEN(RG_PMIM(rg),1)
	    O_TYPE(o) = TY_BOOL
	    if (nargs == 7) {
	        call malloc (O_VALP(o), O_LEN(o), TY_INT)
	        call me_ellipse (Memi[O_VALP(args[1])], Memi[O_VALP(args[2])],
	            Memi[O_VALP(o)], O_LEN(o), Memr[rval1+2], Memr[rval1+3],
		    Memr[rval1+4], Memr[rval1+5], Memr[rval1+6])
	    } else if (nargs == 5) {
	        call malloc (ix, O_LEN(o), TY_INT)
	        call malloc (iy, O_LEN(o), TY_INT)
	        do i = 1, O_LEN(o)
		    Memi[ix+i-1] = i
	        call amovki (RG_OPMV(rg,2), Memi[iy], O_LEN(o))
	        call malloc (O_VALP(o), O_LEN(o), TY_INT)
	        call me_ellipse (Memi[ix], Memi[iy], Memi[O_VALP(o)], O_LEN(o),
	            Memr[rval1], Memr[rval1+1], Memr[rval1+2], Memr[rval1+3],
		    Memr[rval1+4])
	        call mfree (ix, TY_INT)
	        call mfree (iy, TY_INT)
	    } else {
	        O_TYPE(o) = ERR
	    }

	case RG_BOX:
	    O_LEN(o) = IM_LEN(RG_PMIM(rg),1)
	    O_TYPE(o) = TY_BOOL
	    if (nargs == 6) {
	        call malloc (O_VALP(o), O_LEN(o), TY_INT)
	        call me_box (Memi[O_VALP(args[1])], Memi[O_VALP(args[2])],
	            Memi[O_VALP(o)], O_LEN(o), Memr[rval1+2], Memr[rval1+3],
		    Memr[rval1+4], Memr[rval1+5])
	    } else if (nargs == 4) {
	        call malloc (ix, O_LEN(o), TY_INT)
	        call malloc (iy, O_LEN(o), TY_INT)
	        do i = 1, O_LEN(o)
		    Memi[ix+i-1] = i
	        call amovki (RG_OPMV(rg,2), Memi[iy], O_LEN(o))
	        call malloc (O_VALP(o), O_LEN(o), TY_INT)
	        call me_box (Memi[ix], Memi[iy], Memi[O_VALP(o)], O_LEN(o),
	            Memr[rval1], Memr[rval1+1], Memr[rval1+2], Memr[rval1+3])
	        call mfree (ix, TY_INT)
	        call mfree (iy, TY_INT)
	    } else {
	        O_TYPE(o) = ERR
	    }

	case RG_RECTANGLE:
	    O_LEN(o) = IM_LEN(RG_PMIM(rg),1)
	    O_TYPE(o) = TY_BOOL
	    if (nargs == 7) {
	        call malloc (O_VALP(o), O_LEN(o), TY_INT)
	        call me_rectangle (Memi[O_VALP(args[1])], Memi[O_VALP(args[2])],
	            Memi[O_VALP(o)], O_LEN(o), Memr[rval1+2], Memr[rval1+3],
		    Memr[rval1+4], Memr[rval1+5], Memr[rval1+6])
	    } else if (nargs == 5) {
	        call malloc (ix, O_LEN(o), TY_INT)
	        call malloc (iy, O_LEN(o), TY_INT)
	        do i = 1, O_LEN(o)
		    Memi[ix+i-1] = i
	        call amovki (RG_OPMV(rg,2), Memi[iy], O_LEN(o))
	        call malloc (O_VALP(o), O_LEN(o), TY_INT)
	        call me_rectangle (Memi[ix], Memi[iy], Memi[O_VALP(o)],
		    O_LEN(o), Memr[rval1], Memr[rval1+1], Memr[rval1+2],
		    Memr[rval1+3], Memr[rval1+4])
	        call mfree (ix, TY_INT)
	        call mfree (iy, TY_INT)
	    } else {
	        O_TYPE(o) = ERR
	    }

	case RG_POLYGON:
	    O_LEN(o) = IM_LEN(RG_PMIM(rg),1)
	    O_TYPE(o) = TY_BOOL
	    if (nargs < 6) {
		O_TYPE(o) = ERR
	    } else if (O_LEN(args[1]) > 0 && O_LEN(args[2]) > 0) {
	        call malloc (O_VALP(o), O_LEN(o), TY_INT)
	        nver = (nargs - 2) / 2
	        do i = 1, nver
		    Memr[rval2+i-1] = Memr[rval1+2*i+1]
	        do i = 1, nver
		    Memr[rval1+i-1] = Memr[rval1+2*i]
	        call me_polygon (Memi[O_VALP(args[1])], Memi[O_VALP(args[2])],
	            Memi[O_VALP(o)], O_LEN(o), Memr[rval1], Memr[rval2], nver)
	    } else {
	        call malloc (ix, O_LEN(o), TY_INT)
	        call malloc (iy, O_LEN(o), TY_INT)
	        do i = 1, O_LEN(o)
		    Memi[ix+i-1] = i
	        call amovki (RG_OPMV(rg,2), Memi[iy], O_LEN(o))
	        nver = nargs / 2
	        do i = 1, nver
		    Memr[rval2+i-1] = Memr[rval1+2*i-1]
	        do i = 1, nver
		    Memr[rval1+i-1] = Memr[rval1+2*i-2]
	        call malloc (O_VALP(o), O_LEN(o), TY_INT)
	        call me_polygon (Memi[ix], Memi[iy], Memi[O_VALP(o)], O_LEN(o),
	            Memr[rval1], Memr[rval2], nver)
	        call mfree (ix, TY_INT)
	        call mfree (iy, TY_INT)
	    }

	case RG_COLS:
	    O_LEN(o) = IM_LEN(RG_PMIM(rg),1)
	    O_TYPE(o) = TY_BOOL
	    if (nargs == 2) {
	        call malloc (O_VALP(o), O_LEN(o), TY_INT)
	        call me_cols (Memi[O_VALP(args[1])], Memi[O_VALP(o)], O_LEN(o),
	            O_VALC(args[2]))
	    } else if (nargs == 1) {
	        call malloc (ix, O_LEN(o), TY_INT)
	        do i = 1, O_LEN(o)
		    Memi[ix+i-1] = i
	        call malloc (O_VALP(o), O_LEN(o), TY_INT)
	        call me_cols (Memi[ix], Memi[O_VALP(o)], O_LEN(o),
		    O_VALC(args[1]))
	        call mfree (ix, TY_INT)
	    } else {
		O_TYPE(o) = ERR
	    }

	case RG_LINES:
	    O_LEN(o) = IM_LEN(RG_PMIM(rg),1)
	    O_TYPE(o) = TY_BOOL
	    if (nargs == 2) {
	        call malloc (O_VALP(o), O_LEN(o), TY_INT)
	        call me_lines (Memi[O_VALP(args[1])], Memi[O_VALP(o)], O_LEN(o),
	            O_VALC(args[2]))
	    } else if (nargs == 1) {
        	call malloc (ix, O_LEN(o), TY_INT)
	        call amovki (RG_OPMV(rg,2), Memi[ix], O_LEN(o))
	        call malloc (O_VALP(o), O_LEN(o), TY_INT)
	        call me_lines (Memi[ix], Memi[O_VALP(o)], O_LEN(o),
		O_VALC(args[1]))
	        call mfree (ix, TY_INT)
	    } else {
		O_TYPE(o) = ERR
	    }

	case RG_VECTOR:
	    O_LEN(o) = IM_LEN(RG_PMIM(rg),1)
	    O_TYPE(o) = TY_BOOL
	    if (nargs == 7) {
	        call malloc (O_VALP(o), O_LEN(o), TY_INT)
	        call me_vector (Memi[O_VALP(args[1])], Memi[O_VALP(args[2])],
	            Memi[O_VALP(o)], O_LEN(o), Memr[rval1+2], Memr[rval1+3],
	            Memr[rval1+4], Memr[rval1+5], Memr[rval1+6])
	    } else if (nargs == 5) {
	        call malloc (ix, O_LEN(o), TY_INT)
	        call malloc (iy, O_LEN(o), TY_INT)
	        do i = 1, O_LEN(o)
		    Memi[ix+i-1] = i
	        call amovki (RG_OPMV(rg,2), Memi[iy], O_LEN(o))
	        call malloc (O_VALP(o), O_LEN(o), TY_INT)
	        call me_vector (Memi[ix], Memi[iy], Memi[O_VALP(o)], O_LEN(o),
	            Memr[rval1], Memr[rval1+1], Memr[rval1+2], Memr[rval1+3],
		    Memr[rval1+4])
	        call mfree (ix, TY_INT)
	        call mfree (iy, TY_INT)
	    } else {
		O_TYPE(o) = ERR
	    }

	case RG_PIE:
	    O_LEN(o) = IM_LEN(RG_PMIM(rg),1)
	    O_TYPE(o) = TY_BOOL
	    if (nargs == 6) {
	        call malloc (O_VALP(o), O_LEN(o), TY_INT)
	        call me_pie (Memi[O_VALP(args[1])], Memi[O_VALP(args[2])],
	            Memi[O_VALP(o)], O_LEN(o), Memr[rval1+2], Memr[rval1+3],
	            Memr[rval1+4], Memr[rval1+5], IM_LEN(RG_PMIM(rg),1),
		    IM_LEN(RG_PMIM(rg),2))
	    } else if (nargs == 4) {
	        call malloc (ix, O_LEN(o), TY_INT)
	        call malloc (iy, O_LEN(o), TY_INT)
	        do i = 1, O_LEN(o)
		    Memi[ix+i-1] = i
	        call amovki (RG_OPMV(rg,2), Memi[iy], O_LEN(o))
	        call malloc (O_VALP(o), O_LEN(o), TY_INT)
	        call me_pie (Memi[ix], Memi[iy], Memi[O_VALP(o)], O_LEN(o),
	            Memr[rval1], Memr[rval1+1], Memr[rval1+2], Memr[rval1+3],
		    IM_LEN(RG_PMIM(rg),1), IM_LEN(RG_PMIM(rg),2))
	        call mfree (ix, TY_INT)
	        call mfree (iy, TY_INT)
	    } else {
		O_TYPE(o) = ERR
	    }

	case RG_CANNULUS:
	    O_LEN(o) = IM_LEN(RG_PMIM(rg),1)
	    O_TYPE(o) = TY_BOOL
	    if (nargs == 6) {
	        call malloc (O_VALP(o), O_LEN(o), TY_INT)
	        call me_cannulus (Memi[O_VALP(args[1])], Memi[O_VALP(args[2])],
	            Memi[O_VALP(o)], O_LEN(o), Memr[rval1+2], Memr[rval1+3],
		    Memr[rval1+4], Memr[rval1+5])
	    } else if (nargs == 4) {
	        call malloc (ix, O_LEN(o), TY_INT)
	        call malloc (iy, O_LEN(o), TY_INT)
	        do i = 1, O_LEN(o)
		    Memi[ix+i-1] = i
	        call amovki (RG_OPMV(rg,2), Memi[iy], O_LEN(o))
	        call malloc (O_VALP(o), O_LEN(o), TY_INT)
	        call me_cannulus (Memi[ix], Memi[iy], Memi[O_VALP(o)], O_LEN(o),
	            Memr[rval1], Memr[rval1+1], Memr[rval1+2], Memr[rval1+3])
	        call mfree (ix, TY_INT)
	        call mfree (iy, TY_INT)
	    } else {
		O_TYPE(o) = ERR
	    }

	case RG_EANNULUS:
	    O_LEN(o) = IM_LEN(RG_PMIM(rg),1)
	    O_TYPE(o) = TY_BOOL
	    if (nargs == 8) {
	        call malloc (O_VALP(o), O_LEN(o), TY_INT)
	        call me_eannulus (Memi[O_VALP(args[1])], Memi[O_VALP(args[2])],
	            Memi[O_VALP(o)], O_LEN(o), Memr[rval1+2], Memr[rval1+3],
		    Memr[rval1+4], Memr[rval1+5], Memr[rval1+6], Memr[rval1+7])
	    } else if (nargs == 6) {
	        call malloc (ix, O_LEN(o), TY_INT)
	        call malloc (iy, O_LEN(o), TY_INT)
	        do i = 1, O_LEN(o)
		    Memi[ix+i-1] = i
	        call amovki (RG_OPMV(rg,2), Memi[iy], O_LEN(o))
	        call malloc (O_VALP(o), O_LEN(o), TY_INT)
	        call me_eannulus (Memi[ix], Memi[iy], Memi[O_VALP(o)], O_LEN(o),
	            Memr[rval1], Memr[rval1+1], Memr[rval1+2], Memr[rval1+3],
		    Memr[rval1+4], Memr[rval1+5])
	        call mfree (ix, TY_INT)
	        call mfree (iy, TY_INT)
	    } else {
		O_TYPE(o) = ERR
	    }

	case RG_RANNULUS:
	    O_LEN(o) = IM_LEN(RG_PMIM(rg),1)
	    O_TYPE(o) = TY_BOOL
	    if (nargs == 8) {
	        call malloc (O_VALP(o), O_LEN(o), TY_INT)
	        call me_rannulus (Memi[O_VALP(args[1])], Memi[O_VALP(args[2])],
	            Memi[O_VALP(o)], O_LEN(o), Memr[rval1+2], Memr[rval1+3],
		    Memr[rval1+4], Memr[rval1+5], Memr[rval1+6], Memr[rval1+7])
	    } else if (nargs == 6) {
	        call malloc (ix, O_LEN(o), TY_INT)
	        call malloc (iy, O_LEN(o), TY_INT)
	        do i = 1, O_LEN(o)
		    Memi[ix+i-1] = i
	        call amovki (RG_OPMV(rg,2), Memi[iy], O_LEN(o))
	            call malloc (O_VALP(o), O_LEN(o), TY_INT)
	        call me_rannulus (Memi[ix], Memi[iy], Memi[O_VALP(o)], O_LEN(o),
	            Memr[rval1], Memr[rval1+1], Memr[rval1+2], Memr[rval1+3],
		    Memr[rval1+4], Memr[rval1+5])
	        call mfree (ix, TY_INT)
	        call mfree (iy, TY_INT)
	    } else {
		O_TYPE(o) = ERR
	    }

	case RG_PANNULUS:
	    O_LEN(o) = IM_LEN(RG_PMIM(rg),1)
	    O_TYPE(o) = TY_BOOL
	    if (nargs < 7) {
		O_TYPE(o) = ERR
	    } else if (O_LEN(args[1]) > 0 && O_LEN(args[2]) > 0) {
	        call malloc (O_VALP(o), O_LEN(o), TY_INT)
	        width = Memr[rval1+2]
	        nver = (nargs - 3) / 2
	        do i = 1, nver
		    #Memr[rval2+i-1] = Memr[rval1+2*i+1]
		    Memr[rval2+i-1] = Memr[rval1+2*i+2]
	        do i = 1, nver
		    #Memr[rval1+i-1] = Memr[rval1+2*i+2]
		    Memr[rval1+i-1] = Memr[rval1+2*i+1]
	        call salloc (orval1, nver, TY_REAL)
	        call salloc (orval2, nver, TY_REAL)
	        call me_pyexpand (Memr[rval1], Memr[rval2], Memr[orval1],
		    Memr[orval2], nver, width)
	        call me_apolygon (Memi[O_VALP(args[1])], Memi[O_VALP(args[2])],
	            Memi[O_VALP(o)], O_LEN(o), Memr[rval1], Memr[rval2],
		    Memr[orval1], Memr[orval2], nver)
	    } else {
	        call malloc (ix, O_LEN(o), TY_INT)
	        call malloc (iy, O_LEN(o), TY_INT)
	        do i = 1, O_LEN(o)
		    Memi[ix+i-1] = i
	        call amovki (RG_OPMV(rg,2), Memi[iy], O_LEN(o))
	        call malloc (O_VALP(o), O_LEN(o), TY_INT)
	        width = Memr[rval1]
	        nver = (nargs - 1) / 2
	        do i = 1, nver
		    Memr[rval2+i-1] = Memr[rval1+2*i]
	        do i = 1, nver
		    Memr[rval1+i-1] = Memr[rval1+2*i-1]
	        call salloc (orval1, nver, TY_REAL)
	        call salloc (orval2, nver, TY_REAL)
	        call me_pyexpand (Memr[rval1], Memr[rval2], Memr[orval1],
		    Memr[orval2], nver, width)
	        call me_apolygon (Memi[ix], Memi[iy], Memi[O_VALP(o)], O_LEN(o),
	            Memr[rval1], Memr[rval2], Memr[orval1], Memr[orval2], nver)
	        call mfree (ix, TY_INT)
	        call mfree (iy, TY_INT)
	    }

	case RG_POINT:
	    O_LEN(o) = IM_LEN(RG_PMIM(rg),1)
	    O_TYPE(o) = TY_BOOL
	    if (nargs == 4) {
	        call malloc (O_VALP(o), O_LEN(o), TY_INT)
	        call me_point (Memi[O_VALP(args[1])], Memi[O_VALP(args[2])],
	            Memi[O_VALP(o)], O_LEN(o), Memr[rval1+2], Memr[rval1+3])
	    } else if (nargs == 2) {
	        call malloc (ix, O_LEN(o), TY_INT)
	        call malloc (iy, O_LEN(o), TY_INT)
	        do i = 1, O_LEN(o)
		    Memi[ix+i-1] = i
	        call amovki (RG_OPMV(rg,2), Memi[iy], O_LEN(o))
	        call malloc (O_VALP(o), O_LEN(o), TY_INT)
	        call me_point (Memi[ix], Memi[iy], Memi[O_VALP(o)], O_LEN(o),
	            Memr[rval1], Memr[rval1+1])
	        call mfree (ix, TY_INT)
	        call mfree (iy, TY_INT)
	    } else {
	        O_TYPE(o) = ERR
	    }
	default:
	    O_TYPE(o) = ERR
	}

	call sfree (sp)
end

