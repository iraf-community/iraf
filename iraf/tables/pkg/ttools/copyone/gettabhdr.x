# GETTABHDR -- Read a keyword from an table header into a string
#
# B.Simon	14-Aug-87	First Code
# B.Simon	12-Dec-94	Added error check

procedure gettabhdr (hd, keyword, maxch, value, keytype)

pointer hd		# i: Table descriptor
char	keyword[ARB]	# i: Name of header keyword
int	maxch		# i: Maximum length of keyword value
char	value[ARB]	# o: Keyword value
int	keytype		# o: Type of header keyword
#--
int	ip
pointer keyval, sp

int	tabhdrtyp()
errchk	tbhgtt

begin
	call smark (sp)
	call salloc (keyval, maxch, TY_CHAR)

	# Read table header keyword and get datatype

	call tbhgtt (hd, keyword, Memc[keyval], maxch)
	keytype = tabhdrtyp (hd, keyword)

	if (keytype == TY_CHAR) {

	    # Just do a straight copy if the keyword is a string

	    call strcpy (Memc[keyval], value, maxch)

	} else{

	    # Otherwise, strip whitespace from keyword value

	    ip = 1
	    call ctowrd (Memc[keyval], ip, value, maxch)

	    # If boolean, convert to the standard names

	    if (keytype == TY_BOOL) {

		if (value[1] == '1')
		    call strcpy ("yes", value, maxch)
		else
		    call strcpy ("no", value, maxch)
	    }
	}

	call sfree (sp)
	return
end
