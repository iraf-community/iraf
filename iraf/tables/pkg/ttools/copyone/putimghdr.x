define	USRERR		1

# PUTIMGHDR -- Put a keyword given as a string in an image header
#
# B.Simon	14-Aug-87	First Code
# B.Simon	27-Jul-94	Fix bug in addition of double
# B.Simon	21-Jul-97	Workaround for imgftype bug

procedure putimghdr (hd, keyword, value, keytype, add)

pointer	hd		# i: Image descriptor
char	keyword[ARB]	# i: Keyword to put
char	value[ARB]	# i: Keyword value
int	keytype		# i: Keyword type
bool	add		# i: Is adding a new keyword legal?
#--
bool	bvalue
double	dvalue
int	ip, junk, hdrtype
pointer	sp, rp, keyval, errtxt

string	badtyperr	"Type mismatch in header keyword (%s)"
string	notadderr	"Keyword not found in header (%s)"

int	ctod(), idb_findrecord(), imgftype(), stridx()

begin

	call smark (sp)
	call salloc (keyval, SZ_FNAME, TY_CHAR)
	call salloc (errtxt, SZ_LINE, TY_CHAR)

	# Convert keyword value to a double

	ip = 1
	junk = ctod (value, ip, dvalue)

	# If keyword is already in the image header

	if (idb_findrecord (hd, keyword, rp) > 0) {

	    hdrtype = imgftype (hd, keyword)

	    # Extra test to work around bug in imgftype

	    if (hdrtype == TY_BOOL) {
		call imgstr(hd, keyword, Memc[keyval], SZ_FNAME)
		if (Memc[keyval+1] != EOS)
		    hdrtype = TY_CHAR
	    }

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
		call imputb (hd, keyword, bvalue)
	    case TY_CHAR :
		call impstr (hd, keyword, value)
	    case TY_SHORT :
		call imputs (hd, keyword, short(dvalue))
	    case TY_INT :
		call imputi (hd, keyword, int(dvalue))
	    case TY_LONG :
		call imputl (hd, keyword, long(dvalue))
	    case TY_REAL :
		call imputr (hd, keyword, real(dvalue))
	    case TY_DOUBLE :
		call imputd (hd, keyword, dvalue)
	    }

	} else {

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
		call imaddb (hd, keyword, bvalue)
	    case TY_CHAR :
		call imastr (hd, keyword, value)
	    case TY_SHORT :
		call imadds (hd, keyword, short(dvalue))
	    case TY_INT :
		call imaddi (hd, keyword, int(dvalue))
	    case TY_LONG :
		call imaddl (hd, keyword, long(dvalue))
	    case TY_REAL :
		call imaddr (hd, keyword, real(dvalue))
	    case TY_DOUBLE :
		call imaddd (hd, keyword, dvalue)
	    }

	}

	call sfree (sp)
	return
end	
