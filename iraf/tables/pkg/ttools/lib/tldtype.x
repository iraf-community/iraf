define	T_MAXDIM	7	# maximum dimension of array

# tl_dtype -- data type and array size
# Convert integer data type code to a character string.  If the column
# contains arrays, append the length of each axis, e.g. R[25,75].
#
# Phil Hodge,  9-Dec-1994  Moved from tlcol.x to ttools$lib/.
# Phil Hodge, 19-Jul-1995  Add tp to calling sequence (needed for tbciga).

procedure tl_dtype (tp, cp, datatype, nelem, chartyp, maxch)

pointer tp		# i: pointer to table descriptor
pointer cp		# i: pointer to column descriptor
int	datatype	# i: integer code for data type
int	nelem		# i: total array size
char	chartyp[maxch]	# o: data type, possibly with array size
int	maxch		# i: maximum size of chartyp string
#--
int	nchar		# number of characters
int	i		# loop index
int	ndim		# dimension of array
int	axlen[T_MAXDIM]	# length of each axis
int	ip, itoc()
int	strlen()
errchk	tbciga

begin
	if (datatype > 0) {		# numeric or Boolean

	    switch (datatype) {
	    case TY_REAL:
		call strcpy ("R", chartyp, maxch)
	    case TY_DOUBLE:
		call strcpy ("D", chartyp, maxch)
	    case TY_INT:
		call strcpy ("I", chartyp, maxch)
	    case TY_SHORT:
		call strcpy ("S", chartyp, maxch)
	    case TY_BOOL:
		call strcpy ("B", chartyp, maxch)
	    default:
		call error (1, "bad data type in table")
	    }

	} else {			# < 0 ==> char string

	    nchar = -datatype		# length of string
	    call sprintf (chartyp, maxch, "CH*%d")
		call pargi (nchar)
	}

	if (nelem > 1) {

	    # Get the dimension of array and size of each axis.
	    call tbciga (tp, cp, ndim, axlen, T_MAXDIM)

	    call strcat ("[", chartyp, maxch)

	    ip = strlen (chartyp) + 1		# points to EOS

	    do i = 1, ndim-1 {
		nchar = itoc (axlen[i], chartyp[ip], maxch-ip+1)
		call strcat (",", chartyp, maxch)
		ip = ip + nchar + 1
	    }

	    nchar = itoc (axlen[ndim], chartyp[ip], maxch-ip+1)
	    call strcat ("]", chartyp, maxch)
	}
end
