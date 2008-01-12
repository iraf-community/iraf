include	<tbset.h>
define	USRERR		1

# GETTABDAT -- Read an element from a table into a string
#
# B.Simon	17-Aug-1987	First Code
# Phil Hodge	15-May-2002	Add 'format' argument.  ctowrd is a function.

procedure gettabdat (hd, colname, rownum, maxch, format, value, undef, eltype)

pointer hd		# i: Table descriptor
char	colname[ARB]	# i: Table column name
int	rownum		# i: Table row number
int	maxch		# i: Maximum length of element value
bool	format		# i: Format the value using table print format?
char	value[ARB]	# o: Table element value
bool	undef		# o: Is table element undefined?
int	eltype		# o: Type of table element

bool	nullbuf[1]
int	lendata, ip
pointer	colptr[1]
pointer	sp, errtxt, valbuf

double	dval[1]
real	rval[1]
int	ival[1]
bool	bval[1]

string	badnamerr	"Column name not found in table (%s)"
string	unknown_type	"Unknown data type in table"

int	tbcigi()
int	junk, ctowrd()

begin
	# Allocate dynamic memory to hold strings

	call smark (sp)
	call salloc (errtxt, SZ_LINE, TY_CHAR)
	call salloc (valbuf, maxch, TY_CHAR)

	# Get the column pointer from the column name

	call tbcfnd (hd, colname, colptr, 1)

	# If the pointer is NULL, the column was not found

	if (colptr[1] == NULL) {
	    call sprintf (Memc[errtxt], SZ_LINE, badnamerr)
	    call pargstr (colname)
	    call error (USRERR, Memc[errtxt])
	}

	# Get the column data type. Store in eltype

 	eltype = tbcigi (colptr[1], TBL_COL_DATATYPE)
	if (eltype < 0) {
	    lendata = - eltype
	    eltype = TY_CHAR
	}

	# Get the table element as a text string

	if (format || eltype == TY_CHAR) {
	    call tbrgtt (hd, colptr, Memc[valbuf], nullbuf, maxch, 1, rownum)
	} else {
	    switch (eltype) {
	    case TY_BOOL :
		call tbrgtb (hd, colptr, bval, nullbuf, 1, rownum)
		if (bval[1])
		    call strcpy ("yes", Memc[valbuf], maxch)
		else
		    call strcpy ("no", Memc[valbuf], maxch)
	    case TY_SHORT,TY_INT :
		call tbrgti (hd, colptr, ival, nullbuf, 1, rownum)
		call sprintf (Memc[valbuf], maxch, "%d")
		    call pargi (ival)
	    case TY_REAL :
		call tbrgtr (hd, colptr, rval, nullbuf, 1, rownum)
		call sprintf (Memc[valbuf], maxch, "%15.7g")
		    call pargr (rval)
	    case TY_DOUBLE :
		call tbrgtd (hd, colptr, dval, nullbuf, 1, rownum)
		call sprintf (Memc[valbuf], maxch, "%25.16g")
		    call pargd (dval)
	    default :
		call error (1, unknown_type)
	    }
	}

	if (eltype == TY_CHAR) {

	    # Just do a straight copy if the element is a string

	    call strcpy (Memc[valbuf], value, maxch)

	} else{

	    # Otherwise, strip whitespace from element value

	    ip = 1
	    junk = ctowrd (Memc[valbuf], ip, value, maxch)

	}

	undef = nullbuf[1]
	call sfree (sp)

	return
end
