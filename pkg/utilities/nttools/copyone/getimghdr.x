# GETIMGHDR -- Read a keyword from an image header into a string
#
# B.Simon	13-Aug-87	First Code
# B.Simon	12-Dec-94	Added error check
# B.Simon	21-Jul-97	Add extra check to work around imgftype bug

procedure getimghdr (hd, keyword, maxch, value, keytype)

pointer hd		# i: Image descriptor
char	keyword[ARB]	# i: Name of header keyword
int	maxch		# i: Maximum length of keyword value
char	value[ARB]	# o: Keyword value
int	keytype		# o: Type of header keyword
#--
int	imgftype()
errchk	imgstr

begin
	# Read image header keyword and get datatype

	call imgstr (hd, keyword, value, maxch)
	keytype = imgftype (hd, keyword)

	# If boolean, convert to the standard names
	# The check on value[2] is to work around a 
	# bug in imgftype()

	if (value[2] == EOS && keytype == TY_BOOL)
	    if (value[1] == 'T')
		call strcpy ("yes", value, maxch)
	    else
		call strcpy ("no", value, maxch)

	return
end
