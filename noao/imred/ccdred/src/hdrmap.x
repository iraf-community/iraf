.help hdrmap
.nf-----------------------------------------------------------------------------
HDRMAP -- Map translation between task parameters and image header parameters.

In order for tasks to be partially independent of the image header
parameter names used by different instruments and observatories a
translation is made between task parameters and image header
parameters.  This translation is given in a file consisting of the task
parameter name, the image header parameter name, and an optional
default value.  This file is turned into a symbol table.  If the
translation file is not found a null pointer is returned.  The package will
then use the task parameter names directly.  Also if there is no
translation given in the file for a particular parameter it is passed
on directly.  If a parameter is not in the image header then the symbol
table default value, if given, is returned.  This package is layered on
the IMIO header package.

	        hdmopen (fname)
		hdmclose ()
		hdmname (parameter, str, max_char)
		hdmgdef (parameter, str, max_char)
	  y/n = hdmaccf (im, parameter)
		hdmgstr (im, parameter, str, max_char)
	 ival = hdmgeti (im, parameter)
	 rval = hdmgetr (im, parameter)
		hdmpstr (im, parameter, str)
		hdmputi (im, parameter, value)
		hdmputr (im, parameter, value)
		hdmgstp (stp)
		hdmpstp (stp)
		hdmdelf (im, parameter)
		hdmparm (name, parameter, max_char)

hdmopen  -- Open the translation file and map it into a symbol table pointer.
hdmclose -- Close the symbol table pointer.
hdmname  -- Return the image header parameter name.
hdmgdef  -- Get the default value as a string (null if none).
hdmaccf  -- Return whether the image header parameter exists (regardless of
	    whether there is a default value).
hdmgstr  -- Get a string valued parameter.  Return default value if not in the
	    image header.  Return null string if no default or image value.
hdmgeti  -- Get an integer valued parameter.  Return default value if not in
	    the image header and error condition if no default or image value.
hdmgetr  -- Get a real valued parameter.  Return default value if not in
	    the image header or error condition if no default or image value.
hdmpstr  -- Put a string valued parameter in the image header.
hdmputi  -- Put an integer valued parameter in the image header.
hdmputr  -- Put a real valued parameter in the image header.
hdmgstp  -- Get the symbol table pointer to save it while another map is used.
hdmpstp  -- Put the symbol table pointer to restore a map.
hdmdelf  -- Delete a field.
hdmparm  -- Return the parameter name corresponding to an image header name.
.endhelp -----------------------------------------------------------------------

# Symbol table definitions.
define	LEN_INDEX	32		# Length of symtab index
define	LEN_STAB	1024		# Length of symtab string buffer
define	SZ_SBUF		128		# Size of symtab string buffer

define	SZ_NAME		79		# Size of translation symbol name
define	SZ_DEFAULT	79		# Size of default string
define	SYMLEN		80		# Length of symbol structure

# Symbol table structure
define	NAME		Memc[P2C($1)]		# Translation name for symbol
define	DEFAULT		Memc[P2C($1+40)]	# Default value of parameter


# HDMOPEN -- Open the translation file and map it into a symbol table pointer.

procedure hdmopen (fname)

char	fname[ARB]		# Image header map file

int	fd, open(), fscan(), nscan()
pointer	sp, sym, parameter, stopen(), stenter()
include	"hdrmap.com"

begin
	# If the file is not found set the symbol table to null.
	stp = NULL
	iferr (fd = open (fname, READ_ONLY, TEXT_FILE))
	    return

	call smark (sp)
	call salloc (parameter, SZ_NAME, TY_CHAR)

	# Create an empty symbol table.
	stp = stopen (fname, LEN_INDEX, LEN_STAB, SZ_SBUF)

	# Read the file an enter the translations in the symbol table.
	while (fscan(fd) != EOF) {
	    call gargwrd (Memc[parameter], SZ_NAME)
	    if ((nscan() == 0) || (Memc[parameter] == '#'))
		next
	    sym = stenter (stp, Memc[parameter], SYMLEN)
	    call gargwrd (NAME(sym), SZ_NAME)
	    call gargwrd (DEFAULT(sym), SZ_DEFAULT)
	}

	call close (fd)
	call sfree (sp)
end


# HDMCLOSE -- Close the symbol table pointer.

procedure hdmclose ()

include	"hdrmap.com"

begin
	if (stp != NULL)
	    call stclose (stp)
end


# HDMNAME -- Return the image header parameter name

procedure hdmname (parameter, str, max_char)

char	parameter[ARB]		# Parameter name
char	str[max_char]		# String containing mapped parameter name
int	max_char		# Maximum characters in string

pointer	sym, stfind()
include	"hdrmap.com"

begin
	if (stp != NULL)
	    sym = stfind (stp, parameter)
	else
	    sym = NULL

	if (sym != NULL)
	    call strcpy (NAME(sym), str, max_char)
	else
	    call strcpy (parameter, str, max_char)
end


# HDMGDEF -- Get the default value as a string (null string if none).

procedure hdmgdef (parameter, str, max_char)

char	parameter[ARB]		# Parameter name
char	str[max_char]		# String containing default value
int	max_char		# Maximum characters in string

pointer	sym, stfind()
include	"hdrmap.com"

begin
	if (stp != NULL)
	    sym = stfind (stp, parameter)
	else
	    sym = NULL

	if (sym != NULL)
	    call strcpy (DEFAULT(sym), str, max_char)
	else
	    str[1] = EOS
end


# HDMACCF -- Return whether the image header parameter exists (regardless of
# whether there is a default value).

int procedure hdmaccf (im, parameter)

pointer	im			# IMIO pointer
char	parameter[ARB]		# Parameter name

int	imaccf()
pointer	sym, stfind()
include	"hdrmap.com"

begin
	if (stp != NULL)
	    sym = stfind (stp, parameter)
	else
	    sym = NULL

	if (sym != NULL)
	    return (imaccf (im, NAME(sym)))
	else
	    return (imaccf (im, parameter))
end


# HDMGSTR -- Get a string valued parameter.  Return default value if not in
# the image header.  Return null string if no default or image value.

procedure hdmgstr (im, parameter, str, max_char)

pointer	im			# IMIO pointer
char	parameter[ARB]		# Parameter name
char	str[max_char]		# String value to return
int	max_char		# Maximum characters in returned string

pointer	sym, stfind()
include	"hdrmap.com"

begin
	if (stp != NULL)
	    sym = stfind (stp, parameter)
	else
	    sym = NULL

	if (sym != NULL) {
	    iferr (call imgstr (im, NAME(sym), str, max_char))
		call strcpy (DEFAULT(sym), str, max_char)
	} else {
	    iferr (call imgstr (im, parameter, str, max_char))
		str[1] = EOS
	}
end


# HDMGETR -- Get a real valued parameter.  Return default value if not in
# the image header.  Return error condition if no default or image value.

real procedure hdmgetr (im, parameter)

pointer	im			# IMIO pointer
char	parameter[ARB]		# Parameter name

int	ip, ctor()
real	value, imgetr()
pointer	sym, stfind()
include	"hdrmap.com"

begin
	if (stp != NULL)
	    sym = stfind (stp, parameter)
	else
	    sym = NULL

	if (sym != NULL) {
	    iferr (value = imgetr (im, NAME(sym))) {
		ip = 1
		if (ctor (DEFAULT(sym), ip, value) == 0)
		    call error (0, "HDMGETR: No value found")
	    }
	} else
	    value = imgetr (im, parameter)

	return (value)
end


# HDMGETI -- Get an integer valued parameter.  Return default value if not in
# the image header.  Return error condition if no default or image value.

int procedure hdmgeti (im, parameter)

pointer	im			# IMIO pointer
char	parameter[ARB]		# Parameter name

int	ip, ctoi()
int	value, imgeti()
pointer	sym, stfind()
include	"hdrmap.com"

begin
	if (stp != NULL)
	    sym = stfind (stp, parameter)
	else
	    sym = NULL

	if (sym != NULL) {
	    iferr (value = imgeti (im, NAME(sym))) {
		ip = 1
		if (ctoi (DEFAULT(sym), ip, value) == 0)
		    call error (0, "HDMGETI: No value found")
	    }
	} else
	    value = imgeti (im, parameter)

	return (value)
end


# HDMPSTR -- Put a string valued parameter in the image header.

procedure hdmpstr (im, parameter, str)

pointer	im			# IMIO pointer
char	parameter[ARB]		# Parameter name
char	str[ARB]		# String value

int	imaccf(), imgftype()
pointer	sym, stfind()
include	"hdrmap.com"

begin
	if (stp != NULL)
	    sym = stfind (stp, parameter)
	else
	    sym = NULL

	if (sym != NULL) {
	    if (imaccf (im, NAME(sym)) == YES)
	        if (imgftype (im, NAME(sym)) != TY_CHAR)
		    call imdelf (im, NAME(sym))
	    call imastr (im, NAME(sym), str)
	} else {
	    if (imaccf (im, parameter) == YES)
	        if (imgftype (im, parameter) != TY_CHAR)
		    call imdelf (im, parameter)
	    call imastr (im, parameter, str)
	}
end


# HDMPUTI -- Put an integer valued parameter in the image header.

procedure hdmputi (im, parameter, value)

pointer	im			# IMIO pointer
char	parameter[ARB]		# Parameter name
int	value			# Integer value to put

pointer	sym, stfind()
include	"hdrmap.com"

begin
	if (stp != NULL)
	    sym = stfind (stp, parameter)
	else
	    sym = NULL

	if (sym != NULL)
	    call imaddi (im, NAME(sym), value)
	else
	    call imaddi (im, parameter, value)
end


# HDMPUTR -- Put a real valued parameter in the image header.

procedure hdmputr (im, parameter, value)

pointer	im			# IMIO pointer
char	parameter[ARB]		# Parameter name
real	value			# Real value to put

pointer	sym, stfind()
include	"hdrmap.com"

begin
	if (stp != NULL)
	    sym = stfind (stp, parameter)
	else
	    sym = NULL

	if (sym != NULL)
	    call imaddr (im, NAME(sym), value)
	else
	    call imaddr (im, parameter, value)
end


# HDMGSTP -- Get the symbol table pointer to save a translation map.
# The symbol table is restored with HDMPSTP.

procedure hdmgstp (ptr)

pointer	ptr		# Symbol table pointer to return

include	"hdrmap.com"

begin
	ptr = stp
end


# HDMPSTP -- Put a symbol table pointer to restore a header map.
# The symbol table is optained with HDMGSTP.

procedure hdmpstp (ptr)

pointer	ptr		# Symbol table pointer to restore

include	"hdrmap.com"

begin
	stp = ptr
end


# HDMDELF -- Delete a field.  It is an error if the field does not exist.

procedure hdmdelf (im, parameter)

pointer	im			# IMIO pointer
char	parameter[ARB]		# Parameter name

pointer	sym, stfind()
include	"hdrmap.com"

begin
	if (stp != NULL)
	    sym = stfind (stp, parameter)
	else
	    sym = NULL

	if (sym != NULL)
	    call imdelf (im, NAME(sym))
	else
	    call imdelf (im, parameter)
end


# HDMPARAM -- Get parameter given the image header name.

procedure hdmparam (name, parameter, max_char)

char	name[ARB]		# Image header name
char	parameter[max_char]	# Parameter
int	max_char		# Maximum size of parameter string

bool	streq()
pointer	sym, sthead(), stname(), stnext()
include	"hdrmap.com"

begin
	if (stp != NULL)
	    sym = sthead (stp)
	else
	    sym = NULL

	while (sym != NULL) {
	    if (streq (NAME(sym), name)) {
		call strcpy (Memc[stname(stp, sym)], parameter, max_char)
		return
	    }
	    sym = stnext (stp, sym)
	}
	call strcpy (name, parameter, max_char)
end
