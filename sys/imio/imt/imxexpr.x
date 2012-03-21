# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<evexpr.h>
include	<imset.h>
include	<imhdr.h>
include	<ctype.h>
include	<lexnum.h>

define	LEN_USERAREA	28800		# allow for the largest possible header
define	SZ_IMAGENAME	63		# max size of an image name
define	SZ_FIELDNAME	31		# max size of a field name

define	DEBUG		FALSE



# IMX_MATCHEXPR -- Match the open image descriptor against the expression.

bool procedure imx_matchexpr (im, expr)

pointer	im				#I image descriptor
char    expr[ARB]           		#I expression string

bool    stat
char	val[SZ_LINE]
pointer	o

pointer	imt_im			# getop common
char	imt_image[SZ_IMAGENAME]
char	imt_field[SZ_FIELDNAME]
common	/imtgop/ imt_im, imt_image, imt_field

pointer	evexpr()
extern	imx_getop()
int	locpr()
errchk	locpr, evexpr

begin
	call aclrc (val, SZ_LINE)
	call aclrc (imt_image, SZ_IMAGENAME)
	call aclrc (imt_field, SZ_FIELDNAME)

	imt_im = im
	if (expr[1] != EOS) {
	    iferr {
		o = evexpr (expr, locpr (imx_getop), 0)
	    	call imx_encodeop (o, val, SZ_LINE)
	    	stat = O_VALB(o)
	    	call xev_freeop (o)
	    	call mfree (o, TY_STRUCT)
	    } then
		stat = FALSE

	    if (DEBUG) {
		call eprintf ("expr = '%s'  %b\n")
		    call pargstr (expr) ; call pargb (stat)
	    }

	    return (stat)
	}

	return (FALSE)
end


# IMX_SIFMATCH -- Check whether the file is a simple image matching the
# expression.

bool procedure imx_sifmatch (fname, expr)

char    fname[ARB]           		#I image name
char    expr[ARB]           		#I expression string

pointer	im
bool	stat

pointer	immap()
bool	imx_matchexpr (), streq()
errchk	immap

begin
	if (expr[1] == EOS)
	    return (TRUE)

	iferr (im = immap (fname, READ_ONLY, 0)) {
	    return (FALSE)
	}

	if (streq (expr, "yes"))
	    stat = TRUE
	else
	    stat = imx_matchexpr (im, expr)
	call imunmap (im)

	return (stat)
end


# IMX_GETOP -- Satisfy an operand request from EVEXPR.  In this context,
# operand names refer to the fields of the image header.  The following
# special operand names are recognized:
#
#	.		a string literal, returned as the string "."
#	$		the value of the current field
#	$F		the name of the current field
#	$I		the name of the current image
#	$T		the current time, expressed as an integer
#
# The companion procedure HE_GETOPSETIMAGE is used to pass the image pointer
# and image and field names.

procedure imx_getop (operand, o)

char	operand[ARB]		# operand name
pointer	o			# operand (output)

pointer	imt_im			# getop common
char	imt_image[SZ_IMAGENAME]
char	imt_field[SZ_FIELDNAME]
common	/imtgop/ imt_im, imt_image, imt_field
bool	streq()
long	clktime()
errchk	imx_getfield

begin
	if (streq (operand, ".")) {
	    call xev_initop (o, 1, TY_CHAR)
	    call strcpy (".", O_VALC(o), 1)

	} else if (streq (operand, "$")) {
	    call imx_getfield (imt_im, imt_field, o)
	
	} else if (streq (operand, "$F")) {
	    call xev_initop (o, SZ_FIELDNAME, TY_CHAR)
	    call strcpy (imt_field, O_VALC(o), SZ_FIELDNAME)

	} else if (streq (operand, "$I")) {
	    call xev_initop (o, SZ_IMAGENAME, TY_CHAR)
	    call strcpy (imt_image, O_VALC(o), SZ_IMAGENAME)

	} else if (streq (operand, "$T")) {
	    # Assignment of long into int may fail on some systems.  Maybe
	    # should use type string and let database convert to long...

	    call xev_initop (o, 0, TY_INT)
	    O_VALI(o) = clktime (long(0))

	} else
	    call imx_getfield (imt_im, operand, o)
end


# IMX_GETFIELD -- Return the value of the named field of the image header as
# an EVEXPR type operand structure.

procedure imx_getfield (im, field, o)

pointer	im			# image descriptor
char	field[ARB]		# name of field to be returned
pointer	o			# pointer to output operand

bool	imgetb()
int	ftype, imgeti(), imgftype()
real	imgetr()

begin
	iferr {
	    ftype = imgftype (im, field)
	} then {
	    call xev_initop (o, SZ_LINE, TY_CHAR) 	# keyword not found
	    call aclrc (O_VALC(o), SZ_LINE)
	    return
	}

	switch (ftype) {
	case TY_BOOL:
	    call xev_initop (o, 0, TY_BOOL)
	    O_VALB(o) = imgetb (im, field)

	case TY_SHORT, TY_INT, TY_LONG:
	    call xev_initop (o, 0, TY_INT)
	    O_VALI(o) = imgeti (im, field)

	case TY_REAL, TY_DOUBLE, TY_COMPLEX:
	    call xev_initop (o, 0, TY_REAL)
	    O_VALR(o) = imgetr (im, field)

	default:
	    call xev_initop (o, SZ_LINE, TY_CHAR)
	    call imgstr (im, field, O_VALC(o), SZ_LINE)
	}
end


# IMX_ENCODEOP -- Encode an operand as returned by EVEXPR as a string.  EVEXPR
# operands are restricted to the datatypes bool, int, real, and string.

procedure imx_encodeop (o, outstr, maxch)

pointer	o			# operand to be encoded
char	outstr[ARB]		# output string
int	maxch			# max chars in outstr

begin
	switch (O_TYPE(o)) {
	case TY_BOOL:
	    call sprintf (outstr, maxch, "%b")
		call pargb (O_VALB(o))
	case TY_CHAR:
	    call sprintf (outstr, maxch, "%s")
		call pargstr (O_VALC(o))
	case TY_INT:
	    call sprintf (outstr, maxch, "%d")
		call pargi (O_VALI(o))
	case TY_REAL:
	    call sprintf (outstr, maxch, "%g")
		call pargr (O_VALR(o))
	default:
	    call error (1, "unknown expression datatype")
	}
end
