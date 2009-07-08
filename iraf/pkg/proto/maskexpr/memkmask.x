include <mach.h>
include <ctype.h>
include <imhdr.h>
include <imset.h>
include <pmset.h>
include <evvexpr.h>

define	DEF_LINELEN	8192

define	LEN_MSKEXPR	42
define	ME_PMIM		Memp[$1]		# the output mask image
define	ME_REFIM	Memp[$1+1]		# the reference image
define	ME_REFMSK	Memp[$1+2]		# the reference mask image
define	ME_REFDAT	Memp[$1+3]		# current reference image line
define	ME_REFTYPE	Memi[P2I($1+4)]		# the input pixel type
define	ME_REFPMDAT	Memp[$1+5]		# current mask image line
define	ME_PMV		Meml[P2L($1+6)+($2)-1]	# position in mask image
define	ME_REFV		Meml[P2L($1+13)+($2)-1]	# position in reference image
define	ME_REFPMV	Meml[P2L($1+20)+($2)-1]	# position in reference mask


# ME_MKMASK -- Given an expression, a reference image descriptor, a reference
# mask descriptor, the number of dimensions, size of each dimension, and depth
# in bits create a mask image and return an imio pointer to the mask.

pointer procedure me_mkmask (expr, mskname, refim, refmsk, ndim, axlen, depth)

char	expr[ARB]		#I the input expression
char	mskname[ARB]		#I the optional input mask name
pointer	refim			#I the imio pointer to the reference image
pointer	refmsk			#I the imio pointer to the reference mask
int	ndim			#I the number of output mask dimensions
long	axlen[ARB]		#I the size of the output mask
int	depth			#I the pixel depth of the output mask

size_t	sz_val
long	pmaxval, c_1, l_max_long, l_null
pointer	sp, tmpname, pm, pmim, me, obuf, oexpr
int	i, i_val, i_null
short	s_null
size_t	npix
long	j, nlines

pointer pm_create(), im_pmmap(), evvexpr(), immap(), locpr()
pointer	imstatp()
long	imgnli(), imgnll(), imgnlr(), imgnld()
long	impnli(), impnls(), impnll()
extern	me_getop(), me_fcn()
include	<nullptr.inc>

begin
	c_1 = 1
	l_max_long = MAX_LONG
	s_null = NULL
	i_null = NULL
	l_null = NULL

	# Open the output mask and map it as a virtual image or a disk
	# image depending on whether or not you wish to save the mask.
	if (mskname[1] == EOS) {
	    call smark (sp)
	    sz_val = SZ_FNAME
	    call salloc (tmpname, sz_val, TY_CHAR)
	    call mktemp ("tmpmsk", Memc[tmpname], SZ_FNAME)
	    if (refim != NULL) {
	        pmim = im_pmmap (Memc[tmpname], NEW_COPY, refim)
	    } else if (refmsk != NULL) {
	        pmim = im_pmmap (Memc[tmpname], NEW_COPY, refmsk)
	    } else {
	        pmim = im_pmmap (Memc[tmpname], NEW_IMAGE, NULLPTR)
	        IM_NDIM(pmim) = ndim
		sz_val = ndim
	        call amovl (axlen, IM_LEN(pmim,1), sz_val)
	    }
	    call sfree (sp)
	} else {
	    if (refim != NULL) {
		pmim = immap (mskname, NEW_COPY, refim)
	    } else if (refmsk != NULL) {
		pmim = immap (mskname, NEW_COPY, refmsk)
	    } else {
		pmim = immap (mskname, NEW_IMAGE, NULLPTR)
	        IM_NDIM(pmim) = ndim
		sz_val = ndim
	        call amovl (axlen, IM_LEN(pmim,1), sz_val)
	    }
	}
	IM_PIXTYPE(pmim) = TY_INT

	# Initialize the mask.
	pm = imstatp (pmim, IM_PLDES)
	call pl_close (pm)
	pm = pm_create (IM_NDIM(pmim), IM_LEN(pmim,1), depth)
	call imsetp (pmim, IM_PLDES, pm)

	# Determine the mask depth.
	if (depth > 0) {
	    pmaxval = min (depth, PL_MAXDEPTH)
	    pmaxval = 2 ** pmaxval - 1
	} else {
	    pmaxval = 2 ** PL_MAXDEPTH - 1
	}

	# Allocate space for the mask expression structure.
	sz_val = LEN_MSKEXPR
	call calloc (me, sz_val, TY_STRUCT)
	ME_PMIM(me) = pmim
	ME_REFIM(me) = refim
	ME_REFMSK(me) = refmsk

	# Determine the input image type.
	 if (refim != NULL) {
	    switch (IM_PIXTYPE(refim)) {
	    case TY_BOOL, TY_SHORT, TY_INT:
		ME_REFTYPE(me) = TY_INT
	    case TY_LONG:
		ME_REFTYPE(me) = TY_LONG
	    case TY_REAL:
		ME_REFTYPE(me) = TY_REAL
	    case TY_DOUBLE:
		ME_REFTYPE(me) = TY_DOUBLE
	    case TY_COMPLEX:
		ME_REFTYPE(me) = TY_REAL
	    }
	 }

	# Initalize the i/o pointers.
	sz_val = IM_MAXDIM
	call amovkl (c_1, ME_PMV(me,1), sz_val)
	call amovkl (c_1, ME_REFV(me,1), sz_val)
	call amovkl (c_1, ME_REFPMV(me,1), sz_val)

	# Compute the total number of output image lines.
	npix = IM_LEN(pmim,1)
	nlines = 1
	do i = 2, IM_NDIM(pmim)
	    nlines = nlines * IM_LEN(pmim, i)

	# Loop over the mask output image lines which are by default always
	# integer.
	do j = 1, nlines {

	    # Get the correct reference image line.
	    if (refim != NULL) {
		switch (ME_REFTYPE(me)) {
		case TY_INT:
		    if (imgnli (refim, ME_REFDAT(me), ME_REFV(me,1)) == EOF)
			call error (1, "Error reading reference image data")
		case TY_LONG:
		    if (imgnll (refim, ME_REFDAT(me), ME_REFV(me,1)) == EOF)
			call error (1, "Error reading reference image data")
		case TY_REAL:
		    if (imgnlr (refim, ME_REFDAT(me), ME_REFV(me,1)) == EOF)
			call error (1, "Error reading reference image data")
		case TY_DOUBLE:
		    if (imgnld (refim, ME_REFDAT(me), ME_REFV(me,1)) == EOF)
			call error (1, "Error reading reference image data")
		case TY_COMPLEX:
		    if (imgnlr (refim, ME_REFDAT(me), ME_REFV(me,1)) == EOF)
			call error (1, "Error reading reference image data")
		}
	    }

	    # Get the correct reference mask line.
	    if (refmsk != NULL) {
		if (imgnll (refmsk, ME_REFPMDAT(me), ME_REFPMV(me,1)) == EOF)
		    call error (1, "Error reading reference mask data")
	    }

	    # Evalute the expression.
	    oexpr = evvexpr (expr, locpr(me_getop), me, locpr(me_fcn), me, 0)
	    if (O_TYPE(oexpr) == ERR) {
		call eprintf ("Error evaluting expression\n")
		break
	    }

	    # Copy the evaluated expression to the image.
	    if (O_LEN(oexpr) == 0) {
		switch (O_TYPE(oexpr)) {
		case TY_BOOL:
		    if (impnli (pmim, obuf, ME_PMV(me,1)) == EOF)
			call error (2, "Error writing output mask data")
		    i_val = pmaxval
		    call pl_pixropi (i_null, c_1, MAX_INT, Memi[obuf], c_1, 
			i_val, npix, PIX_CLR + PIX_VALUE(O_VALI(oexpr))) 
		case TY_SHORT:
		    if (impnls (pmim, obuf, ME_PMV(me,1)) == EOF)
			call error (2, "Error writing output mask data")
		    i_val = pmaxval
		    call pl_pixrops (s_null, c_1, MAX_SHORT, Mems[obuf], c_1,
		        i_val, npix, PIX_CLR + PIX_VALUE(O_VALS(oexpr))) 
		case TY_INT:
		    if (impnli (pmim, obuf, ME_PMV(me,1)) == EOF)
			call error (2, "Error writing output mask data")
		    i_val = pmaxval
		    call pl_pixropi (i_null, c_1, MAX_INT, Memi[obuf], c_1,
		        i_val, npix, PIX_CLR + PIX_VALUE(O_VALI(oexpr))) 
		case TY_LONG:
		    if (impnll (pmim, obuf, ME_PMV(me,1)) == EOF)
			call error (2, "Error writing output mask data")
		    i_val = PIX_CLR + PIX_VALUE(O_VALL(oexpr))
		    call pl_pixropl (l_null, c_1, l_max_long, Meml[obuf], c_1,
				     pmaxval, npix, i_val)
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
		    if (impnli (pmim, obuf, ME_PMV(me,1)) == EOF)
			call error (2, "Error writing output mask data")
		    i_val = pmaxval
		    call pl_pixropi (Memi[O_VALP(oexpr)], c_1, MAX_INT,
		        Memi[obuf], c_1, i_val, npix, PIX_SRC) 
		case TY_SHORT:
		    if (impnls (pmim, obuf, ME_PMV(me,1)) == EOF)
			call error (2, "Error writing output mask data")
		    i_val = pmaxval
		    call pl_pixrops (Mems[O_VALP(oexpr)], c_1, MAX_SHORT,
		        Mems[obuf], c_1, i_val, npix, PIX_SRC) 
		case TY_INT:
		    if (impnli (pmim, obuf, ME_PMV(me,1)) == EOF)
			call error (2, "Error writing output mask data")
		    i_val = pmaxval
		    call pl_pixropi (Memi[O_VALP(oexpr)], c_1, MAX_INT,
		        Memi[obuf], c_1, i_val, npix, PIX_SRC) 
		case TY_LONG:
		    if (impnll (pmim, obuf, ME_PMV(me,1)) == EOF)
			call error (2, "Error writing output mask data")
		    call pl_pixropl (Meml[O_VALP(oexpr)], c_1, l_max_long,
		        Meml[obuf], c_1, pmaxval, npix, PIX_SRC) 
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
	call mfree (me, TY_STRUCT)

	return (pmim)
end


# ME_GETOP -- Called by evvexpr to fetch an input image operand.

procedure me_getop (me, opname, o)

pointer	me			#I mskexpr descriptor
char	opname[ARB]		#I operand name
pointer	o			#I output operand to be filled in

size_t	sz_val
pointer	sp, param, data, im
long	i, c_1
int	axis
int	imgftype(), btoi()
double	imgetd()
int	imgeti()
long	imgetl()
bool	imgetb()
errchk	malloc
define	err_ 91

begin
	c_1 = 1

	call smark (sp)

	# Reference image operand.
	if ((opname[1] == 'i') && (opname[2] == EOS)) {

	    if (ME_REFIM(me) == NULL)
		goto err_

	    O_TYPE(o) = ME_REFTYPE(me)
	    O_LEN(o) = IM_LEN(ME_REFIM(me), 1)
	    O_FLAGS(o) = 0
	    O_VALP(o) = ME_REFDAT(me)

	    call sfree (sp)
	    return

	# Reference mask operand.
	} else if ((opname[1] == 'm') && (opname[2] == EOS)) {

	    if (ME_REFMSK(me) == NULL)
		goto err_

	    O_TYPE(o) = TY_LONG
	    O_LEN(o) = IM_LEN(ME_REFMSK(me), 1)
	    O_FLAGS(o) = 0
	    O_VALP(o) = ME_REFPMDAT(me)

	    call sfree (sp)
	    return

	# Reference image header parameter operand.
	} else if ((opname[1] == 'i' || opname[1] == 'm') &&
	    (opname[2] == '.')) {

	    if (opname[1] == 'i')
		im = ME_REFIM(me) 
	    else
		im = ME_REFMSK(me) 
	    if (im == NULL)
		goto err_

	    # Get the parameter value and set up operand struct.
	    sz_val = SZ_FNAME
	    call salloc (param, sz_val, TY_CHAR)
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
		    sz_val = SZ_LINE
		    call malloc (O_VALP(o), sz_val, TY_CHAR)
		    call imgstr (im, Memc[param], O_VALC(o), SZ_LINE)
		} then
		    goto err_

	    case TY_SHORT, TY_INT:
		iferr (O_VALI(o) = imgeti (im, Memc[param]))
		    goto err_

	    case TY_LONG:
		iferr (O_VALL(o) = imgetl (im, Memc[param]))
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
		O_TYPE(o) = TY_LONG
		if (IM_LEN(ME_PMIM(me), 1) > 0)
		    O_LEN(o) = IM_LEN(ME_PMIM(me), 1)
		else 
		    O_LEN(o) = DEF_LINELEN
		call malloc (data, O_LEN(o), TY_LONG)
		do i = 1, O_LEN(o)
		    Meml[data+i-1] = i
		O_VALP(o) = data
		O_FLAGS(o) = O_FREEVAL
	    } else {
		O_TYPE(o) = TY_LONG
		if (IM_LEN(ME_PMIM(me), 1) > 0)
		    O_LEN(o) = IM_LEN(ME_PMIM(me), 1)
		else 
		    O_LEN(o) = DEF_LINELEN
		call malloc (data, O_LEN(o), TY_LONG)
		if (axis < 1 || axis > IM_MAXDIM) {
		    call amovkl (c_1, Meml[data], O_LEN(o))
		} else {
		    call amovkl (ME_PMV(me,axis), Meml[data], O_LEN(o))
		}
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

define	ME_FUNCS	"|circle|ellipse|box|rectangle|polygon|cols|lines|\
vector|pie|cannulus|eannulus|rannulus|pannulus|point|"

define	ME_CIRCLE	1
define	ME_ELLIPSE	2
define	ME_BOX		3
define	ME_RECTANGLE	4
define	ME_POLYGON	5
define	ME_COLS		6
define	ME_LINES	7
define	ME_VECTOR	8
define	ME_PIE		9
define	ME_CANNULUS	10
define	ME_EANNULUS	11
define	ME_RANNULUS	12
define	ME_PANNULUS	13
define	ME_POINT	14


# ME_FCN -- Called by evvexpr to execute a mskexpr special function.

procedure me_fcn (me, fcn, args, nargs, o)

pointer	me			#I imexpr descriptor
char	fcn[ARB]		#I function name
pointer	args[ARB]		#I input arguments
int	nargs			#I number of input arguments
pointer	o			#I output operand to be filled in

size_t	sz_val
real	width
pointer	sp, ufunc, lval1, rval1, rval2, orval1, orval2, ix, iy
int	i, ip, func, v_nargs, nver
long	j
int	strdic(), ctor(), ctol()
bool	strne()

begin
	# Allocate working space.
	call smark (sp)
	sz_val = SZ_LINE
	call salloc (ufunc, sz_val, TY_CHAR)

	# Get the function.
	func = strdic (fcn, Memc[ufunc], SZ_LINE, ME_FUNCS)
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
	case ME_POINT, ME_CIRCLE, ME_ELLIPSE, ME_BOX, ME_RECTANGLE, ME_POLYGON:
	    v_nargs = -1
	case ME_CANNULUS, ME_EANNULUS, ME_RANNULUS, ME_PANNULUS:
	    v_nargs = -1
	case ME_COLS, ME_LINES:
	    v_nargs = -1
	case ME_VECTOR, ME_PIE:
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
	if (v_nargs < 0 && nargs < iabs (v_nargs)) {
	    O_TYPE(o) = ERR
	    call sfree (sp)
	    return
	}

	if (func == ME_POLYGON  && nargs < 6) {
	    O_TYPE(o) = ERR
	    call sfree (sp)
	    return
	}

	# Type convert the arguments appropriately. At the moment this is
	# simple if we assume that all the required arguments are real.
	sz_val = nargs
	call salloc (lval1, sz_val, TY_LONG)
	call salloc (rval1, sz_val, TY_REAL)
	call salloc (rval2, sz_val, TY_REAL)
	do i = 1, nargs {
	    switch (O_TYPE(args[i])) {
	    case TY_CHAR:
                ip = 1
                if (ctor (O_VALC(args[i]), ip, Memr[rval1+i-1]) == 0)
                    Memr[rval1+i-1] = 0.
                ip = 1
                if (ctol (O_VALC(args[i]), ip, Meml[lval1+i-1]) == 0)
                    Meml[lval1+i-1] = 0
	    case TY_INT:
		Memr[rval1+i-1] = O_VALI(args[i])
		Meml[lval1+i-1] = O_VALI(args[i])
	    case TY_LONG:
		Memr[rval1+i-1] = O_VALL(args[i])
		Meml[lval1+i-1] = O_VALL(args[i])
	    case TY_REAL:
		Memr[rval1+i-1] = O_VALR(args[i])
		Meml[lval1+i-1] = O_VALR(args[i])
	    case TY_DOUBLE:
		Memr[rval1+i-1] = O_VALD(args[i])
		Meml[lval1+i-1] = O_VALD(args[i])
	    }
	}

	# Evaluate the function. Worry about some duplication of code later.
	switch (func) {

	case ME_CIRCLE:
	    O_LEN(o) = IM_LEN(ME_PMIM(me),1)
	    O_TYPE(o) = TY_BOOL
	    if (nargs == 5) {
	        call malloc (O_VALP(o), O_LEN(o), TY_INT)
	        call me_circle (Meml[lval1+0], Meml[lval1+1],
	            Memi[O_VALP(o)], O_LEN(o), Memr[rval1+2], Memr[rval1+3],
		    Memr[rval1+4])
	    } else if (nargs == 3) {
	        call malloc (ix, O_LEN(o), TY_LONG)
	        call malloc (iy, O_LEN(o), TY_LONG)
	        do j = 1, O_LEN(o)
		    Meml[ix+j-1] = j
	        call amovkl (ME_PMV(me,2), Meml[iy], O_LEN(o))
	        call malloc (O_VALP(o), O_LEN(o), TY_INT)
	        call me_circle (Meml[ix], Meml[iy], Memi[O_VALP(o)], O_LEN(o),
	            Memr[rval1], Memr[rval1+1], Memr[rval1+2])
	        call mfree (ix, TY_LONG)
	        call mfree (iy, TY_LONG)
	    } else {
	        O_TYPE(o) = ERR
	    }

	case ME_ELLIPSE:
	    O_LEN(o) = IM_LEN(ME_PMIM(me),1)
	    O_TYPE(o) = TY_BOOL
	    if (nargs == 7) {
	        call malloc (O_VALP(o), O_LEN(o), TY_INT)
	        call me_ellipse (Meml[lval1+0], Meml[lval1+1],
	            Memi[O_VALP(o)], O_LEN(o), Memr[rval1+2], Memr[rval1+3],
		    Memr[rval1+4], Memr[rval1+5], Memr[rval1+6])
	    } else if (nargs == 5) {
	        call malloc (ix, O_LEN(o), TY_LONG)
	        call malloc (iy, O_LEN(o), TY_LONG)
	        do j = 1, O_LEN(o)
		    Meml[ix+j-1] = j
	        call amovkl (ME_PMV(me,2), Meml[iy], O_LEN(o))
	        call malloc (O_VALP(o), O_LEN(o), TY_INT)
	        call me_ellipse (Meml[ix], Meml[iy], Memi[O_VALP(o)], O_LEN(o),
	            Memr[rval1], Memr[rval1+1], Memr[rval1+2], Memr[rval1+3],
		    Memr[rval1+4])
	        call mfree (ix, TY_LONG)
	        call mfree (iy, TY_LONG)
	    } else {
	        O_TYPE(o) = ERR
	    }

	case ME_BOX:
	    O_LEN(o) = IM_LEN(ME_PMIM(me),1)
	    O_TYPE(o) = TY_BOOL
	    if (nargs == 6) {
	        call malloc (O_VALP(o), O_LEN(o), TY_INT)
	        call me_box (Meml[lval1+0], Meml[lval1+1],
	            Memi[O_VALP(o)], O_LEN(o), Memr[rval1+2], Memr[rval1+3],
		    Memr[rval1+4], Memr[rval1+5])
	    } else if (nargs == 4) {
	        call malloc (ix, O_LEN(o), TY_LONG)
	        call malloc (iy, O_LEN(o), TY_LONG)
	        do j = 1, O_LEN(o)
		    Meml[ix+j-1] = j
	        call amovkl (ME_PMV(me,2), Meml[iy], O_LEN(o))
	        call malloc (O_VALP(o), O_LEN(o), TY_INT)
	        call me_box (Meml[ix], Meml[iy], Memi[O_VALP(o)], O_LEN(o),
	            Memr[rval1], Memr[rval1+1], Memr[rval1+2], Memr[rval1+3])
	        call mfree (ix, TY_LONG)
	        call mfree (iy, TY_LONG)
	    } else {
	        O_TYPE(o) = ERR
	    }

	case ME_RECTANGLE:
	    O_LEN(o) = IM_LEN(ME_PMIM(me),1)
	    O_TYPE(o) = TY_BOOL
	    if (nargs == 7) {
	        call malloc (O_VALP(o), O_LEN(o), TY_INT)
	        call me_rectangle (Meml[lval1+0], Meml[lval1+1],
	            Memi[O_VALP(o)], O_LEN(o), Memr[rval1+2], Memr[rval1+3],
		    Memr[rval1+4], Memr[rval1+5], Memr[rval1+6])
	    } else if (nargs == 5) {
	        call malloc (ix, O_LEN(o), TY_LONG)
	        call malloc (iy, O_LEN(o), TY_LONG)
	        do j = 1, O_LEN(o)
		    Meml[ix+j-1] = j
	        call amovkl (ME_PMV(me,2), Meml[iy], O_LEN(o))
	        call malloc (O_VALP(o), O_LEN(o), TY_INT)
	        call me_rectangle (Meml[ix], Meml[iy], Memi[O_VALP(o)],
		    O_LEN(o), Memr[rval1], Memr[rval1+1], Memr[rval1+2],
		    Memr[rval1+3], Memr[rval1+4])
	        call mfree (ix, TY_LONG)
	        call mfree (iy, TY_LONG)
	    } else {
	        O_TYPE(o) = ERR
	    }

	case ME_POLYGON:
	    O_LEN(o) = IM_LEN(ME_PMIM(me),1)
	    O_TYPE(o) = TY_BOOL
	    if (nargs < 6) {
		O_TYPE(o) = ERR
	    } else if (O_LEN(args[1]) > 0 && O_LEN(args[2]) > 0) {
	        call malloc (O_VALP(o), O_LEN(o), TY_INT)
	        nver = (nargs - 2) / 2
	        do i = 1, nver
		    #Memr[rval2+i-1] = Memr[rval1+2*i]
		    Memr[rval2+i-1] = Memr[rval1+2*i+1]
	        do i = 1, nver
		    #Memr[rval1+i-1] = Memr[rval1+2*i+1]
		    Memr[rval1+i-1] = Memr[rval1+2*i]
	        call me_polygon (Meml[lval1+0], Meml[lval1+1],
	            Memi[O_VALP(o)], O_LEN(o), Memr[rval1], Memr[rval2], nver)
	    } else {
	        call malloc (ix, O_LEN(o), TY_LONG)
	        call malloc (iy, O_LEN(o), TY_LONG)
	        do j = 1, O_LEN(o)
		    Meml[ix+j-1] = j
	        call amovkl (ME_PMV(me,2), Meml[iy], O_LEN(o))
	        nver = nargs / 2
	        do i = 1, nver
		    Memr[rval2+i-1] = Memr[rval1+2*i-1]
	        do i = 1, nver
		    Memr[rval1+i-1] = Memr[rval1+2*i-2]
	        call malloc (O_VALP(o), O_LEN(o), TY_INT)
	        call me_polygon (Meml[ix], Meml[iy], Memi[O_VALP(o)], O_LEN(o),
	            Memr[rval1], Memr[rval2], nver)
	        call mfree (ix, TY_LONG)
	        call mfree (iy, TY_LONG)
	    }

	case ME_COLS:
	    O_LEN(o) = IM_LEN(ME_PMIM(me),1)
	    O_TYPE(o) = TY_BOOL
	    if (nargs == 2) {
	        call malloc (O_VALP(o), O_LEN(o), TY_INT)
	        call me_cols (Meml[lval1+0], Memi[O_VALP(o)], O_LEN(o),
	            O_VALC(args[2]))
	    } else if (nargs == 1) {
	        call malloc (ix, O_LEN(o), TY_LONG)
	        do j = 1, O_LEN(o)
		    Meml[ix+j-1] = j
	        call malloc (O_VALP(o), O_LEN(o), TY_INT)
	        call me_cols (Meml[ix], Memi[O_VALP(o)], O_LEN(o),
		    O_VALC(args[1]))
	        call mfree (ix, TY_LONG)
	    } else {
		O_TYPE(o) = ERR
	    }

	case ME_LINES:
	    O_LEN(o) = IM_LEN(ME_PMIM(me),1)
	    O_TYPE(o) = TY_BOOL
	    if (nargs == 2) {
	        call malloc (O_VALP(o), O_LEN(o), TY_INT)
	        call me_lines (Meml[lval1+0], Memi[O_VALP(o)], O_LEN(o),
	            O_VALC(args[2]))
	    } else if (nargs == 1) {
	        call malloc (ix, O_LEN(o), TY_LONG)
	        call amovkl (ME_PMV(me,2), Meml[ix], O_LEN(o))
	        call malloc (O_VALP(o), O_LEN(o), TY_INT)
	        call me_lines (Meml[ix], Memi[O_VALP(o)], O_LEN(o),
			       O_VALC(args[1]))
	        call mfree (ix, TY_LONG)
	    } else {
		O_TYPE(o) = ERR
	    }

	case ME_VECTOR:
	    O_LEN(o) = IM_LEN(ME_PMIM(me),1)
	    O_TYPE(o) = TY_BOOL
	    if (nargs == 7) {
	        call malloc (O_VALP(o), O_LEN(o), TY_INT)
	        call me_vector (Meml[lval1+0], Meml[lval1+1],
	            Memi[O_VALP(o)], O_LEN(o), Memr[rval1+2], Memr[rval1+3],
	            Memr[rval1+4], Memr[rval1+5], Memr[rval1+6])
	    } else if (nargs == 5) {
	        call malloc (ix, O_LEN(o), TY_LONG)
	        call malloc (iy, O_LEN(o), TY_LONG)
	        do j = 1, O_LEN(o)
		    Meml[ix+j-1] = j
	        call amovkl (ME_PMV(me,2), Meml[iy], O_LEN(o))
	        call malloc (O_VALP(o), O_LEN(o), TY_INT)
	        call me_vector (Meml[ix], Meml[iy], Memi[O_VALP(o)], O_LEN(o),
	            Memr[rval1], Memr[rval1+1], Memr[rval1+2], Memr[rval1+3],
		    Memr[rval1+4])
	        call mfree (ix, TY_LONG)
	        call mfree (iy, TY_LONG)
	    } else {
		O_TYPE(o) = ERR
	    }

	case ME_PIE:
	    O_LEN(o) = IM_LEN(ME_PMIM(me),1)
	    O_TYPE(o) = TY_BOOL
	    if (nargs == 6) {
	        call malloc (O_VALP(o), O_LEN(o), TY_INT)
	        call me_pie (Meml[lval1+0], Meml[lval1+1],
	            Memi[O_VALP(o)], O_LEN(o), Memr[rval1+2], Memr[rval1+3],
	            Memr[rval1+4], Memr[rval1+5], IM_LEN(ME_PMIM(me),1),
		    IM_LEN(ME_PMIM(me),2))
	    } else if (nargs == 4) {
	        call malloc (ix, O_LEN(o), TY_LONG)
	        call malloc (iy, O_LEN(o), TY_LONG)
	        do j = 1, O_LEN(o)
		    Meml[ix+j-1] = j
	        call amovkl (ME_PMV(me,2), Meml[iy], O_LEN(o))
	        call malloc (O_VALP(o), O_LEN(o), TY_INT)
	        call me_pie (Meml[ix], Meml[iy], Memi[O_VALP(o)], O_LEN(o),
	            Memr[rval1], Memr[rval1+1], Memr[rval1+2], Memr[rval1+3],
		    IM_LEN(ME_PMIM(me),1), IM_LEN(ME_PMIM(me),2))
	        call mfree (ix, TY_LONG)
	        call mfree (iy, TY_LONG)
	    } else {
		O_TYPE(o) = ERR
	    }

	case ME_CANNULUS:
	    O_LEN(o) = IM_LEN(ME_PMIM(me),1)
	    O_TYPE(o) = TY_BOOL
	    if (nargs == 6) {
	        call malloc (O_VALP(o), O_LEN(o), TY_INT)
	        call me_cannulus (Meml[lval1+0], Meml[lval1+1],
	            Memi[O_VALP(o)], O_LEN(o), Memr[rval1+2], Memr[rval1+3],
		    Memr[rval1+4], Memr[rval1+5])
	    } else if (nargs == 4) {
	        call malloc (ix, O_LEN(o), TY_LONG)
	        call malloc (iy, O_LEN(o), TY_LONG)
	        do j = 1, O_LEN(o)
		    Meml[ix+j-1] = j
	        call amovkl (ME_PMV(me,2), Meml[iy], O_LEN(o))
	        call malloc (O_VALP(o), O_LEN(o), TY_INT)
	        call me_cannulus (Meml[ix], Meml[iy], Memi[O_VALP(o)], O_LEN(o),
	            Memr[rval1], Memr[rval1+1], Memr[rval1+2], Memr[rval1+3])
	        call mfree (ix, TY_LONG)
	        call mfree (iy, TY_LONG)
	    } else {
		O_TYPE(o) = ERR
	    }

	case ME_EANNULUS:
	    O_LEN(o) = IM_LEN(ME_PMIM(me),1)
	    O_TYPE(o) = TY_BOOL
	    if (nargs == 8) {
	        call malloc (O_VALP(o), O_LEN(o), TY_INT)
	        call me_eannulus (Meml[lval1+0], Meml[lval1+1],
	            Memi[O_VALP(o)], O_LEN(o), Memr[rval1+2], Memr[rval1+3],
		    Memr[rval1+4], Memr[rval1+5], Memr[rval1+6], Memr[rval1+7])
	    } else if (nargs == 6) {
	        call malloc (ix, O_LEN(o), TY_LONG)
	        call malloc (iy, O_LEN(o), TY_LONG)
	        do j = 1, O_LEN(o)
		    Meml[ix+j-1] = j
	        call amovkl (ME_PMV(me,2), Meml[iy], O_LEN(o))
	        call malloc (O_VALP(o), O_LEN(o), TY_INT)
	        call me_eannulus (Meml[ix], Meml[iy], Memi[O_VALP(o)], O_LEN(o),
	            Memr[rval1], Memr[rval1+1], Memr[rval1+2], Memr[rval1+3],
		    Memr[rval1+4], Memr[rval1+5])
	        call mfree (ix, TY_LONG)
	        call mfree (iy, TY_LONG)
	    } else {
		O_TYPE(o) = ERR
	    }

	case ME_RANNULUS:
	    O_LEN(o) = IM_LEN(ME_PMIM(me),1)
	    O_TYPE(o) = TY_BOOL
	    if (nargs == 8) {
	        call malloc (O_VALP(o), O_LEN(o), TY_INT)
	        call me_rannulus (Meml[lval1+0], Meml[lval1+1],
	            Memi[O_VALP(o)], O_LEN(o), Memr[rval1+2], Memr[rval1+3],
		    Memr[rval1+4], Memr[rval1+5], Memr[rval1+6], Memr[rval1+7])
	    } else if (nargs == 6) {
	        call malloc (ix, O_LEN(o), TY_LONG)
	        call malloc (iy, O_LEN(o), TY_LONG)
	        do j = 1, O_LEN(o)
		    Meml[ix+j-1] = j
	        call amovkl (ME_PMV(me,2), Meml[iy], O_LEN(o))
	            call malloc (O_VALP(o), O_LEN(o), TY_INT)
	        call me_rannulus (Meml[ix], Meml[iy], Memi[O_VALP(o)], O_LEN(o),
	            Memr[rval1], Memr[rval1+1], Memr[rval1+2], Memr[rval1+3],
		    Memr[rval1+4], Memr[rval1+5])
	        call mfree (ix, TY_LONG)
	        call mfree (iy, TY_LONG)
	    } else {
		O_TYPE(o) = ERR
	    }

	case ME_PANNULUS:
	    O_LEN(o) = IM_LEN(ME_PMIM(me),1)
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
	        sz_val = nver
	        call salloc (orval1, sz_val, TY_REAL)
	        call salloc (orval2, sz_val, TY_REAL)
	        call me_pyexpand (Memr[rval1], Memr[rval2], Memr[orval1],
		    Memr[orval2], nver, width)
	        call me_apolygon (Meml[lval1+0], Meml[lval1+1],
	            Memi[O_VALP(o)], O_LEN(o), Memr[rval1], Memr[rval2],
		    Memr[orval1], Memr[orval2], nver)
	    } else {
	        call malloc (ix, O_LEN(o), TY_LONG)
	        call malloc (iy, O_LEN(o), TY_LONG)
	        do j = 1, O_LEN(o)
		    Meml[ix+j-1] = j
	        call amovkl (ME_PMV(me,2), Meml[iy], O_LEN(o))
	        call malloc (O_VALP(o), O_LEN(o), TY_INT)
	        width = Memr[rval1]
	        nver = (nargs - 1) / 2
	        do i = 1, nver
		    Memr[rval2+i-1] = Memr[rval1+2*i]
	        do i = 1, nver
		    Memr[rval1+i-1] = Memr[rval1+2*i-1]
	        sz_val = nver
	        call salloc (orval1, sz_val, TY_REAL)
	        call salloc (orval2, sz_val, TY_REAL)
	        call me_pyexpand (Memr[rval1], Memr[rval2], Memr[orval1],
		    Memr[orval2], nver, width)
	        call me_apolygon (Meml[ix], Meml[iy], Memi[O_VALP(o)], O_LEN(o),
	            Memr[rval1], Memr[rval2], Memr[orval1], Memr[orval2], nver)
	        call mfree (ix, TY_LONG)
	        call mfree (iy, TY_LONG)
	    }

	case ME_POINT:
	    O_LEN(o) = IM_LEN(ME_PMIM(me),1)
	    O_TYPE(o) = TY_BOOL
	    if (nargs == 4) {
	        call malloc (O_VALP(o), O_LEN(o), TY_INT)
	        call me_point (Meml[lval1+0], Meml[lval1+1],
	            Memi[O_VALP(o)], O_LEN(o), Memr[rval1+2], Memr[rval1+3])
	    } else if (nargs == 2) {
	        call malloc (ix, O_LEN(o), TY_LONG)
	        call malloc (iy, O_LEN(o), TY_LONG)
	        do j = 1, O_LEN(o)
		    Meml[ix+j-1] = j
	        call amovkl (ME_PMV(me,2), Meml[iy], O_LEN(o))
	        call malloc (O_VALP(o), O_LEN(o), TY_INT)
	        call me_point (Meml[ix], Meml[iy], Memi[O_VALP(o)], O_LEN(o),
	            Memr[rval1], Memr[rval1+1])
	        call mfree (ix, TY_LONG)
	        call mfree (iy, TY_LONG)
	    } else {
	        O_TYPE(o) = ERR
	    }
	default:
	    O_TYPE(o) = ERR
	}

	call sfree (sp)
end

