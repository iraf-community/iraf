include	<tbset.h>
define	USRERR		1

# PUTTABDAT -- Write a value passed as a string into a table element
#
# B.Simon	17-Aug-87	First Code

procedure puttabdat (hd, colname, rownum, value, undef, eltype)

pointer hd		# i: Table descriptor
char	colname[ARB]	# i: Table column name
int	rownum		# i: Table row number
char	value[ARB]	# i: Table element value
bool	undef		# i: Is table element undefined?
int	eltype		# i: Type of table element

bool	bvalue[1]
double	dvalue[1]
int	ivalue[1]
pointer	colptr[1]
real	rvalue[1]

int	coltype, lendata, ip, junk, maxch
pointer	sp, errtxt

string	badtyperr	"Type mismatch in table column (%s)"
string	badnamerr	"Column name not found in table (%s)"

int	ctod()
int	stridx(), strlen(), tbcigi()

begin
	# Allocate dynamic memory to hold strings

	call smark (sp)
	call salloc (errtxt, SZ_LINE, TY_CHAR)

	# Get the column pointer from the column name

	call tbcfnd (hd, colname, colptr, 1)

	# If the pointer is NULL, the column was not found

	if (colptr[1] == NULL) {
	    call sprintf (Memc[errtxt], SZ_LINE, badnamerr)
	    call pargstr (colname)
	    call error (USRERR, Memc[errtxt])
	}

	# Get the column data type. Store in coltype

	coltype = tbcigi (colptr[1], TBL_COL_DATATYPE)
	if (coltype < 0) {
	    lendata = - coltype
	    coltype = TY_CHAR
	}

	if (undef)

	    # Set table element to undefined

	    call tbrudf (hd, colptr, 1, rownum)

	else {

	    # Convert element value to a double

	    ip = 1
	    junk = ctod (value, ip, dvalue[1])

	    # Check for illegal type conversions

	    if ((coltype == TY_BOOL && eltype != TY_BOOL) ||
		(!(coltype == eltype || coltype == TY_CHAR) &&
		  (eltype == TY_BOOL || eltype == TY_CHAR)   ) ) {

		call sprintf (Memc[errtxt], SZ_LINE, badtyperr)
		call pargstr (colname)
		call error (USRERR, Memc[errtxt])

	    }

	    # Use the proper procedure to write the new element value

	    switch (coltype) {
	    case TY_BOOL :
		bvalue[1] = stridx (value[1], "TtYy") > 0
		call tbrptb (hd, colptr, bvalue, 1, rownum)
	    case TY_CHAR :
		maxch = strlen (value) + 1
		call tbrptt (hd, colptr, value, maxch, 1, rownum)
	    case TY_SHORT,TY_INT,TY_LONG :
		ivalue[1] = int (dvalue[1])
		call tbrpti (hd, colptr, ivalue, 1, rownum)
	    case TY_REAL :
		rvalue[1] = real (dvalue[1])
		call tbrptr (hd, colptr, rvalue, 1, rownum)
	    case TY_DOUBLE :
		call tbrptd (hd, colptr, dvalue, 1, rownum)
	    }
	}

	call sfree (sp)

	return
end
