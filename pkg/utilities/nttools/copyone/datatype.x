include	<lexnum.h>

# DATATYPE -- Determine the data type of a character string token
#
# B.Simon	13-Aug-87	First Code
# B.Simon	27-Jul-94	Distinguish between double and real
# B.Simon	15-Sep-94	Add check of token length for TY_CHAR
# B.Simon	15-Dec-94	Replace test for double

int procedure datatype (token)

char	token[ARB]	# i: Character string token
#--
int	ic, dtype, nchar, ndigit
pointer	sp, utoken


bool	streq()
int	nowhite(), lexnum(), is_double()

begin
	# Convert token to upper case with no whitespace

	call smark (sp)
	call salloc (utoken, SZ_LINE, TY_CHAR)

	nchar = nowhite (token, Memc[utoken], SZ_LINE)
	call strupr (Memc[utoken])

	# Determine if token is a number

	ic = 1
	switch (lexnum (Memc[utoken], ic, ndigit)) {
	case LEX_OCTAL :
	    dtype = TY_INT
	case LEX_DECIMAL :
	    dtype = TY_INT
	case LEX_HEX :
	    dtype = TY_CHAR
	case LEX_REAL :
	    dtype = TY_REAL
	case LEX_NONNUM :
	    dtype = TY_CHAR
	}

	# Check number of digits parsed against length of string
	# if it is shorter, we have a character string that starts 
	# with a digit

	if (ndigit != nchar)
	    dtype = TY_CHAR

	# Determine if string is boolean

	switch (Memc[utoken]) {
	case 'T':
	    if (streq (Memc[utoken],"T") || streq (Memc[utoken],"TRUE"))
		dtype = TY_BOOL
	case 'F':
	    if (streq (Memc[utoken],"F") || streq (Memc[utoken],"FALSE"))
		dtype = TY_BOOL
	case 'Y':
	    if (streq (Memc[utoken],"Y") || streq (Memc[utoken],"YES"))
		dtype = TY_BOOL
	case 'N':
	    if (streq (Memc[utoken],"N") || streq (Memc[utoken],"NO"))
		dtype = TY_BOOL
	}

	# Determine if datatype is real or double by the number of digits
	# and / or the presence of "D"

	if (dtype == TY_REAL)
	    dtype = is_double (Memc[utoken])

	call sfree (sp)
	return (dtype)
end

