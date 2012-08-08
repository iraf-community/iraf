define	USRERR		1

# PUTTABHDR -- Put a keyword given as a string in a table header
#
# B.Simon	14-Aug-87	First Code
# B.Simon	27-Jul-94	Fix bug in addition of double
# B.Simon	10-Nov-95	Add check for history keyword

procedure puttabhdr (hd, keyword, value, keytype, add)

pointer	hd		# i: Table descriptor
char	keyword[ARB]	# i: Keyword to put
char	value[ARB]	# i: Keyword value
int	keytype		# i: Keyword type
bool	add		# i: Is adding a new keyword legal?
#--
bool	bvalue
double	dvalue
int	ip, junk, hdrtype, keynum
pointer	sp, errtxt

string	badtyperr	"Type mismatch in header keyword (%s)"
string	notadderr	"Keyword not found in header (%s)"

bool	tbhisc()
int	ctod(), tabhdrtyp(), stridx()

begin

	call smark (sp)
	call salloc (errtxt, SZ_LINE, TY_CHAR)

	# Convert keyword value to a double

	ip = 1
	junk = ctod (value, ip, dvalue)

	# If keyword is not already in the table header
	# or this is a history keyword

	call tbhfkw (hd, keyword, keynum)
	if ( keynum == 0 || tbhisc (keyword)) {

	    # Check to see if it legal to add a new keyword

	    if (! add) {
		call sprintf (Memc[errtxt], SZ_LINE, notadderr)
		call pargstr (keyword)
		call error (USRERR, Memc[errtxt])
	    }

	    # Create the new keyword and set its value

	    switch (keytype) {
	    case TY_BOOL :
		bvalue = stridx (value[1], "TtYy") > 0
		call tbhadb (hd, keyword, bvalue)
	    case TY_CHAR :
		call tbhadt (hd, keyword, value)
	    case TY_SHORT,TY_INT,TY_LONG :
		call tbhadi (hd, keyword, int(dvalue))
	    case TY_REAL :
		call tbhadr (hd, keyword, real(dvalue))
	    case TY_DOUBLE :
		call tbhadd (hd, keyword, dvalue)
	    }

	} else {

	    hdrtype = tabhdrtyp (hd, keyword)

	    # Check for illegal type conversions

	    if ((hdrtype == TY_BOOL && keytype != TY_BOOL) ||
		(!(hdrtype == keytype || hdrtype == TY_CHAR) &&
		  (keytype == TY_BOOL || keytype == TY_CHAR)   ) ) {

		call sprintf (Memc[errtxt], SZ_LINE, badtyperr)
		call pargstr (keyword)
		call error (USRERR, Memc[errtxt])

	    }

	    # Use the proper procedure to write the new keyword value

	    switch (hdrtype) {
	    case TY_BOOL :
		bvalue = stridx (value[1], "TtYy") > 0
		call tbhptb (hd, keyword, bvalue)
	    case TY_CHAR :
		call tbhptt (hd, keyword, value)
	    case TY_SHORT,TY_INT,TY_LONG :
		call tbhpti (hd, keyword, int(dvalue))
	    case TY_REAL :
		call tbhptr (hd, keyword, real(dvalue))
	    case TY_DOUBLE :
		call tbhptd (hd, keyword, dvalue)
	    }

	}

	call sfree (sp)
	return
end	
