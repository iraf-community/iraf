include	<lexnum.h>
include	<ctype.h>
include	<mach.h>

# FM_CHECK -- Check a string against a data type
#
# B.Simon	28-Mar-91	Modified to check INDEF correctly

bool procedure fm_check (datatype, str)

int	datatype	# i: Datatype to check
char	str[ARB]	# i: String to be checked
#--
bool	match
double	strval
int	ic, nc, lextype, strtype
pointer	sp, temp

string	yorn  "|yes|no|"

bool	streq()
int	strlen(), lexnum(), ctod(), strdic()

begin
	# Don't check null strings

	if (str[1] == EOS)
	    return (true)

	call smark (sp)
	call salloc (temp, SZ_LINE, TY_CHAR)

	if (datatype < 0)

	    # The only check on string types is that they not exceed their
	    # maximum length

	    match = strlen (str) <= -(datatype)

	else {

	    # Get the data type of the string
	    # Reduce this to character, integer or real
	    # Get the value of the string if it is not character

	    if (streq (str, "INDEF")) {
		strtype = datatype
		strval = 0.0

	    } else {
		ic = 1
		lextype = lexnum (str, ic, nc)

		for (ic = ic + nc; IS_WHITE(str[ic]); ic = ic + 1)
		    ;
		if (str[ic] != EOS)
		    lextype = LEX_NONNUM

		if (lextype == LEX_HEX || lextype == LEX_NONNUM) {
		    strtype = TY_CHAR
		    strval = 0.0
		} else {
		    if (lextype == LEX_REAL)
			strtype = TY_REAL
		    else
			strtype = TY_INT

		    ic = 1
		    nc = ctod (str, ic, strval)
		    strval = abs (strval)
		}
	    }

	    # See if the string matches the expected datatype

	    switch (datatype) {
	    case TY_BOOL:
		match = strdic (str, Memc[temp], SZ_LINE, yorn) > 0
	    case TY_CHAR:
		match = strlen (str) <= 1
	    case TY_SHORT:
		match = strtype == TY_INT && strval <= MAX_SHORT
	    case TY_INT:
		match = strtype == TY_INT && strval <= MAX_INT
	    case TY_LONG:
		match = strtype == TY_INT && strval <= MAX_LONG
	    case TY_REAL:
		match = strtype != TY_CHAR && strval <= MAX_REAL
	    case TY_DOUBLE:
		match = strtype != TY_CHAR && strval <= MAX_DOUBLE
	    default:
		match = true
	    }
	}

	call sfree (sp)
	return (match)
end
