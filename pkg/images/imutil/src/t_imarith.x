# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<error.h>
include	<lexnum.h>

define	ADD	1			# Opcodes.
define	SUB	2
define	MUL	3
define	DIV	4
define	MIN	5
define	MAX	6

# T_IMARITH -- Simple image arithmetic.
#
# For each pixel in each image compute:
#
#	operand1 op operand2 = result
#
# Do the operations as efficiently as possible.  Allow operand1 or operand2
# to be a constant.  Allow resultant image to have the same name as an
# operand image.  Allow lists for the operands and the results.
# Allow one of the operands to have extra dimensions but require that the
# common dimensions are of the same length.

procedure t_imarith ()

int	list1				# Operand1 list
int	list2				# Operand2 list
int	list3				# Result list
int	op				# Operator
bool	verbose				# Verbose option
bool	noact				# Noact option
double	c1				# Constant for operand1
double	c2				# Constant for operand2
double	divzero				# Zero divide replacement
int	pixtype				# Output pixel datatype
int	calctype			# Datatype for calculations

int	i, j, pixtype1, pixtype2
short	sc1, sc2, sdz
int	hlist
double	dval1, dval2
pointer	im1, im2, im3
pointer	sp, operand1, operand2, result, imtemp
pointer	opstr, dtstr, field, title, hparams

int	imtopenp(), imtgetim(), imtlen(), imofnlu(), imgnfn()
double	clgetd(), imgetd()
bool	clgetb(), streq()
int	clgwrd()
int	gctod(), lexnum()
pointer	immap()
errchk	immap, imgetd, imputd

begin
	# Allocate memory for strings.
	call smark (sp)
	call salloc (operand1, SZ_FNAME, TY_CHAR)
	call salloc (operand2, SZ_FNAME, TY_CHAR)
	call salloc (result, SZ_FNAME, TY_CHAR)
	call salloc (imtemp, SZ_FNAME, TY_CHAR)
	call salloc (opstr, SZ_FNAME, TY_CHAR)
	call salloc (dtstr, SZ_FNAME, TY_CHAR)
	call salloc (field, SZ_FNAME, TY_CHAR)
	call salloc (title, SZ_IMTITLE, TY_CHAR)
	call salloc (hparams, SZ_LINE, TY_CHAR)

	# Get the operands and the operator.
	list1 = imtopenp ("operand1")
	op = clgwrd ("op", Memc[opstr], SZ_FNAME, ",+,-,*,/,min,max,")
	list2 = imtopenp ("operand2")
	list3 = imtopenp ("result")

	# Get the rest of the options.
	call clgstr ("hparams", Memc[hparams], SZ_LINE)
	verbose = clgetb ("verbose")
	noact = clgetb ("noact")
	if (op == DIV)
	    divzero = clgetd ("divzero")

	# Check the number of elements.
	if (((imtlen (list1) != 1) && (imtlen (list1) != imtlen (list3))) ||
	    ((imtlen (list2) != 1) && (imtlen (list2) != imtlen (list3)))) {
	    call imtclose (list1)
	    call imtclose (list2)
	    call imtclose (list3)
	    call error (1, "Wrong number of elements in the operand lists")
	}

	# Do each operation.
	while (imtgetim (list3, Memc[result], SZ_FNAME) != EOF) {
	    if (imtgetim (list1, Memc[imtemp], SZ_FNAME) != EOF)
		call strcpy (Memc[imtemp], Memc[operand1], SZ_FNAME)
	    if (imtgetim (list2, Memc[imtemp], SZ_FNAME) != EOF)
		call strcpy (Memc[imtemp], Memc[operand2], SZ_FNAME)

	    # Image sections in the output are not allowed.
	    call imgsection (Memc[result], Memc[field], SZ_FNAME)
	    if (Memc[field] != EOS) {
		call eprintf (
	        "imarith: image sections in the output are not allowed (%s)\n")
		    call pargstr (Memc[result])
		next
	    }

	    # To allow purely numeric file names first test if the operand
	    # is a file.  If it is not then attempt to interpret the operand
	    # as a numerical constant.  Otherwise it is an error.
	    iferr {
		im1 = immap (Memc[operand1], READ_ONLY, 0)
		pixtype1 = IM_PIXTYPE(im1)
	    } then {
		i = 1
	        j = gctod (Memc[operand1], i, c1)
		if ((Memc[operand1+i-1]!=EOS) && (Memc[operand1+i-1]!=' ')) {
		    call eprintf ("%s is not an image or a number\n")
			call pargstr (Memc[operand1])
		    next
		}

		i = 1
		pixtype1 = lexnum (Memc[operand1], i, j)
		switch (pixtype1) {
		case LEX_REAL:
		    pixtype1 = TY_REAL
		default:
		    pixtype1 = TY_SHORT
		}
		im1 = NULL
	    }

	    iferr {
		im2 = immap (Memc[operand2], READ_ONLY, 0)
		pixtype2 = IM_PIXTYPE(im2)
	    } then {
		i = 1
	        j = gctod (Memc[operand2], i, c2)
		if ((Memc[operand2+i-1]!=EOS) && (Memc[operand2+i-1]!=' ')) {
		    call eprintf ("%s is not an image or a number\n")
			call pargstr (Memc[operand2])
		    if (im1 != NULL)
			call imunmap (im1)
		    next
		}

		i = 1
		pixtype2 = lexnum (Memc[operand2], i, j)
		switch (pixtype2) {
		case LEX_REAL:
		    pixtype2 = TY_REAL
		default:
		    pixtype2 = TY_SHORT
		}
		im2 = NULL
	    }

	    # Determine the output pixel datatype and calculation datatype.
	    call ima_set (pixtype1, pixtype2, op, pixtype, calctype)

	    # If verbose or noact print the operation.
	    if (verbose || noact) {
	        call printf ("IMARITH:\n  Operation = %s\n")
		    call pargstr (Memc[opstr])
	        call printf ("  Operand1 = %s\n  Operand2 = %s\n")
		    call pargstr (Memc[operand1])
		    call pargstr (Memc[operand2])
	        call printf ("  Result = %s\n  Result pixel type = %s\n")
		    call pargstr (Memc[result])
		    call dtstring (pixtype, Memc[dtstr], SZ_FNAME)
		    call pargstr (Memc[dtstr])
	        call printf ("  Calculation type = %s\n")
		    call dtstring (calctype, Memc[dtstr], SZ_FNAME)
		    call pargstr (Memc[dtstr])
		if (op == DIV) {
		    call printf (
			"  Replacement value for division by zero  = %g\n")
			call pargd (divzero)
		}
	    }

	    # Do the operation if the no act switch is not set.
	    if (!noact) {
	        # Check the two operands have the same dimension lengths
		# over the same dimensions.
	        if ((im1 != NULL) && (im2 != NULL)) {
		    j = OK
		    do i = 1, min (IM_NDIM (im1), IM_NDIM (im2))
	    	        if (IM_LEN (im1, i) != IM_LEN (im2, i))
			    j = ERR
		    if (j == ERR) {
			call imunmap (im1)
			call imunmap (im2)
	                call eprintf (
			    "Input images have different dimensions\n")
			next
		    }
		}

	        # Create a temporary output image as a copy of one of the
		# operand images (the one with the highest dimension).
		# This allows the resultant image to have
		# the same name as one of the operand images.
	        if ((im1 != NULL) && (im2 != NULL)) {
	            call xt_mkimtemp (Memc[operand1], Memc[result],
			Memc[imtemp], SZ_FNAME)
		    if (streq (Memc[result], Memc[imtemp]))
	                call xt_mkimtemp (Memc[operand2], Memc[result],
			    Memc[imtemp], SZ_FNAME)
		    if (IM_NDIM(im1) >= IM_NDIM(im2))
	                im3 = immap (Memc[result], NEW_COPY, im1)
		    else
	                im3 = immap (Memc[result], NEW_COPY, im2)
	        } else if (im1 != NULL) {
	            call xt_mkimtemp (Memc[operand1], Memc[result],
			Memc[imtemp], SZ_FNAME)
	            im3 = immap (Memc[result], NEW_COPY, im1)
	        } else if (im2 != NULL) {
	            call xt_mkimtemp (Memc[operand2], Memc[result],
			Memc[imtemp], SZ_FNAME)
	            im3 = immap (Memc[result], NEW_COPY, im2)
	        } else
	            call error (0, "No operand images")

		# Set the result image title and pixel datatype.
		call clgstr ("title", Memc[title], SZ_IMTITLE)
		if (Memc[title] != EOS)
		    call strcpy (Memc[title], IM_TITLE (im3), SZ_IMTITLE)
		IM_PIXTYPE (im3) = pixtype

		# Call the appropriate procedure to do the arithmetic
		# efficiently.
	        switch (calctype) {
	        case TY_SHORT:
		    sc1 = c1
		    sc2 = c2
	            switch (op) {
	            case ADD:
	                call ima_adds (im1, im2, im3, sc1, sc2)
	            case SUB:
	                call ima_subs (im1, im2, im3, sc1, sc2)
	            case MUL:
	                call ima_muls (im1, im2, im3, sc1, sc2)
	            case DIV:
			sdz = divzero
	                call ima_divs (im1, im2, im3, sc1, sc2, sdz)
	            case MIN:
	                call ima_mins (im1, im2, im3, sc1, sc2)
	            case MAX:
	                call ima_maxs (im1, im2, im3, sc1, sc2)
	            }
	        case TY_INT:
	            switch (op) {
	            case ADD:
	                call ima_addi (im1, im2, im3, int (c1), int (c2))
	            case SUB:
	                call ima_subi (im1, im2, im3, int (c1), int (c2))
	            case MUL:
	                call ima_muli (im1, im2, im3, int (c1), int (c2))
	            case DIV:
	                call ima_divi (im1, im2, im3, int (c1), int (c2),
			    int (divzero))
	            case MIN:
	                call ima_mini (im1, im2, im3, int (c1), int (c2))
	            case MAX:
	                call ima_maxi (im1, im2, im3, int (c1), int (c2))
	            }
	        case TY_LONG:
	            switch (op) {
	            case ADD:
	                call ima_addl (im1, im2, im3, long (c1), long (c2))
	            case SUB:
	                call ima_subl (im1, im2, im3, long (c1), long (c2))
	            case MUL:
	                call ima_mull (im1, im2, im3, long (c1), long (c2))
	            case DIV:
	                call ima_divl (im1, im2, im3, long (c1), long (c2),
			    long (divzero))
	            case MIN:
	                call ima_minl (im1, im2, im3, long (c1), long (c2))
	            case MAX:
	                call ima_maxl (im1, im2, im3, long (c1), long (c2))
	            }
	        case TY_REAL:
	            switch (op) {
	            case ADD:
	                call ima_addr (im1, im2, im3, real (c1), real (c2))
	            case SUB:
	                call ima_subr (im1, im2, im3, real (c1), real (c2))
	            case MUL:
	                call ima_mulr (im1, im2, im3, real (c1), real (c2))
	            case DIV:
	                call ima_divr (im1, im2, im3, real (c1), real (c2),
			    real (divzero))
	            case MIN:
	                call ima_minr (im1, im2, im3, real (c1), real (c2))
	            case MAX:
	                call ima_maxr (im1, im2, im3, real (c1), real (c2))
	            }
	        case TY_DOUBLE:
	            switch (op) {
	            case ADD:
	                call ima_addd (im1, im2, im3, double(c1), double(c2))
	            case SUB:
	                call ima_subd (im1, im2, im3, double(c1), double(c2))
	            case MUL:
	                call ima_muld (im1, im2, im3, double(c1), double(c2))
	            case DIV:
	                call ima_divd (im1, im2, im3, double(c1), double(c2),
			    double(divzero))
	            case MIN:
	                call ima_mind (im1, im2, im3, double(c1), double(c2))
	            case MAX:
	                call ima_maxd (im1, im2, im3, double(c1), double(c2))
	            }
		}

	        # Do the header parameters.
	        iferr {
		    ifnoerr (dval1 = imgetd (im3, "CCDMEAN"))
		        call imdelf (im3, "CCDMEAN")

	            hlist = imofnlu (im3, Memc[hparams])
		    while (imgnfn (hlist, Memc[field], SZ_FNAME) != EOF) {
	                if (im1 != NULL)
	                    dval1 = imgetd (im1, Memc[field])
		        else
			    dval1 = c1
	                if (im2 != NULL)
	                    dval2 = imgetd (im2, Memc[field])
		        else
			    dval2 = c2

		        switch (op) {
	                case ADD:
			    call imputd (im3, Memc[field], dval1 + dval2)
	                case SUB:
			    call imputd (im3, Memc[field], dval1 - dval2)
	                case MUL:
			    call imputd (im3, Memc[field], dval1 * dval2)
	                case DIV:
			    if (dval2 == 0.) {
				call eprintf (
		    "WARNING: Division by zero in header keyword (%s)\n")
				    call pargstr (Memc[field])
			    } else
				call imputd (im3, Memc[field], dval1 / dval2)
	                case MIN:
			    call imputd (im3, Memc[field], min (dval1, dval2))
	                case MAX:
			    call imputd (im3, Memc[field], max (dval1, dval2))
		        }
		    }
		    call imcfnl (hlist)
	        } then
		    call erract (EA_WARN)
	    }

	    # Unmap images and release the temporary output image.
	    if (im1 != NULL)
	        call imunmap (im1)
	    if (im2 != NULL)
	        call imunmap (im2)
	    if (!noact) {
	        call imunmap (im3)
	        call xt_delimtemp (Memc[result], Memc[imtemp])
	    }
	}

	call imtclose (list1)
	call imtclose (list2)
	call imtclose (list3)
	call sfree (sp)
end


# IMA_SET -- Determine the output image pixel type and the calculation
# datatype.  The default pixel types are based on the highest arithmetic
# precendence of the input images or constants.  Division requires
# a minimum of real.

procedure ima_set (pixtype1, pixtype2, op, pixtype, calctype)

int	pixtype1			# Pixel datatype of operand 1
int	pixtype2			# Pixel datatype of operand 2
int	pixtype				# Pixel datatype of resultant image
int	op				# Operation
int	calctype			# Pixel datatype for calculations

char	line[1]
int	max_type

begin
	# Determine maximum precedence datatype.
	switch (pixtype1) {
	case TY_SHORT:
	    if (op == DIV)
		max_type = TY_REAL
	    else if (pixtype2 == TY_USHORT)
		max_type = TY_LONG
	    else
	        max_type = pixtype2
	case TY_USHORT:
	    if (op == DIV)
		max_type = TY_REAL
	    else if ((pixtype2 == TY_SHORT) || (pixtype2 == TY_USHORT))
		max_type = TY_LONG
	    else
		max_type = pixtype2
	case TY_INT:
	    if (op == DIV)
		max_type = TY_REAL
	    else if ((pixtype2 == TY_SHORT) || (pixtype2 == TY_USHORT))
		max_type = pixtype1
	    else
		max_type = pixtype2
	case TY_LONG:
	    if (op == DIV)
		max_type = TY_REAL
	    else if ((pixtype2 == TY_SHORT) || (pixtype2 == TY_USHORT) ||
	        (pixtype2 == TY_INT))
		max_type = pixtype1
	    else
		max_type = pixtype2
	case TY_REAL:
	    if (pixtype2 == TY_DOUBLE)
		max_type = pixtype2
	    else
		max_type = pixtype1
	case TY_DOUBLE:
	    max_type = pixtype1
	}

	# Set calculation datatype.
	call clgstr ("calctype", line, 1)
	switch (line[1]) {
	case '1':
	    if (pixtype1 == TY_USHORT)
		calctype = TY_LONG
	    else
	        calctype = pixtype1
	case '2':
	    if (pixtype2 == TY_USHORT)
		calctype = TY_LONG
	    else
	        calctype = pixtype2
	case EOS:
	    calctype = max_type
	case 's':
	    calctype = TY_SHORT
	case 'u':
	    calctype = TY_LONG
	case 'i':
	    calctype = TY_INT
	case 'l':
	    calctype = TY_LONG
	case 'r':
	    calctype = TY_REAL
	case 'd':
	    calctype = TY_DOUBLE
	default:
	    call error (6, "Unrecognized datatype")
	}

	# Set output pixel datatype.
	call clgstr ("pixtype", line, 1)
	switch (line[1]) {
	case '1':
	    pixtype = pixtype1
	case '2':
	    pixtype = pixtype2
	case EOS:
	    pixtype = calctype
	case 's':
	    pixtype = TY_SHORT
	case 'u':
	    pixtype = TY_USHORT
	case 'i':
	    pixtype = TY_INT
	case 'l':
	    pixtype = TY_LONG
	case 'r':
	    pixtype = TY_REAL
	case 'd':
	    pixtype = TY_DOUBLE
	default:
	    call error (6, "Unrecognized dataype")
	}
end
