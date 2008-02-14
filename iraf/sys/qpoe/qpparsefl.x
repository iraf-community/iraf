# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"qpoe.h"
include	"qpex.h"

# QP_PARSEFL -- Parse the field list, or declarations string for a user
# defined datatype (structure or domain).
#
# Syntax:	{ type1, type2, ..., typeN }
#
# e.g.,		{d,s:x,s:y,s,s,s,s}	(Rosat/PROS event structure)
#
# where the TYPEi are primitive types, e.g., "r" or "real", "i" or "int",
# etc.  Selected fields may have ":x" or ":y" appended to indicate that these
# are the default coordinate fields to be used for position based extraction.
# Fields will be automatically aligned as necessary, and the computed structure
# size will be forced to be an integral multiple of the largest datatype
# within the structure, to ensure proper alignment in arrays of the structures.

int procedure qp_parsefl (qp, fieldlist, dd)

pointer	qp			#I QPOE descriptor
char	fieldlist[ARB]		#I field list defining new datatype (domain)
pointer	dd			#U pointer to domain descriptor

pointer	sp, tokbuf, dsym, in
int	nfields, offset, maxsize, xfield, yfield, token, dtype, fsize

pointer	qp_opentext()
int	qp_gettok(), qp_nexttok(), sizeof(), qp_dtype()
errchk	qp_gettok, qp_opentext, qp_nexttok
string	qperr "QPOE structdef"
define	nextfield_ 91

begin
	call smark (sp)
	call salloc (tokbuf, SZ_TOKBUF, TY_CHAR)

	# Open declarations string for non macro expanded token input.
	in = qp_opentext (NULL, fieldlist)

	# Advance to structure terms list.
	while (qp_gettok (in, Memc[tokbuf], SZ_TOKBUF) != EOF)
	    if (Memc[tokbuf] == '{')
		break

	nfields = 0
	offset  = 0
	maxsize = 0
	xfield  = 0
	yfield  = 0

	# Process the structure terms list.
	repeat {
	    token = qp_gettok (in, Memc[tokbuf], SZ_TOKBUF)

	    switch (token) {		# {
	    case EOF, '}':
		break

	    case TOK_IDENTIFIER:
		# Get field datatype and size.
		dtype = qp_dtype (qp, Memc[tokbuf], dsym)
		if (dtype < TY_BOOL || dtype > TY_COMPLEX) {
		    call eprintf ("%s: bad field type `%s'\n")
			call pargstr (qperr)
			call pargstr (Memc[tokbuf])
		    goto nextfield_
		} else
		    fsize = sizeof (dtype)

		# Output field descriptor.
		nfields = nfields + 1
		if (nfields > MAX_FIELDS) {
		    call eprintf ("%s: too many fields `%s'\n")
			call pargstr (qperr)
			call pargstr (Memc[tokbuf])
		    break
		}
		DD_FOFFSET(dd,nfields) = (offset + fsize-1) / fsize
		DD_FTYPE(dd,nfields)   = dtype

		# Update structure size parameters.
		offset = (DD_FOFFSET(dd,nfields) * fsize) + fsize
		maxsize = max (maxsize, fsize)

		# Process any :[XY] field modifiers.
		if (qp_nexttok(in) == ':') {
		    repeat {
			token = qp_gettok (in, Memc[tokbuf], SZ_TOKBUF)
			switch (Memc[tokbuf]) {
			case ':':
			    next
			case 'x':
			    if (xfield != 0) {
				call eprintf ("%s: duplicate X field `%s'\n")
				    call pargstr (qperr)
				    call pargstr (Memc[tokbuf])
			    }
			    xfield = nfields
			    break
			case 'y':
			    if (yfield != 0) {
				call eprintf ("%s: duplicate Y field `%s'\n")
				    call pargstr (qperr)
				    call pargstr (Memc[tokbuf])
			    }
			    yfield = nfields
			    break
			default:
			    call eprintf ("%s: unknown : field modifier `%s'\n")
				call pargstr (qperr)
				call pargstr (Memc[tokbuf])
			}
		    }
		    goto nextfield_
		}
	    case ',':
		next
	    default:
		call eprintf ("%s: unexpected token `%s'\n")
		    call pargstr (qperr)
		    call pargstr (Memc[tokbuf])
	    }

nextfield_
	    # Read and discard tokens until we get to the next field.
	    while (qp_gettok (in, Memc[tokbuf], SZ_TOKBUF) != EOF)
		if (Memc[tokbuf] == ',')
		    break
	}

	# Complete the domain descriptor initialization.
	DD_NFIELDS(dd) = nfields
	DD_XFIELD(dd)  = xfield
	DD_YFIELD(dd)  = yfield

	# Pad the struct size to an integral multiple of the max field size.
	if (nfields > 0) {
	    maxsize = max (SZ_STRUCT, maxsize)
	    DD_STRUCTLEN(dd) = (offset+maxsize-1)/maxsize*maxsize / SZ_STRUCT
	} else
	    DD_STRUCTLEN(dd) = 0

	call qp_closetext (in)
	call sfree (sp)

	return (nfields)
end
