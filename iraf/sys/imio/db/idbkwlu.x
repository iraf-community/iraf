# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>
include	<imhdr.h>

# IDB_KWLOOKUP -- Look up a keyword in the dictionary of standard header
# keywords, returning the magic integer code of the keyword or zero.

int procedure idb_kwlookup (key)

char	key[ARB]		# keyword to be looked up
int	index, ip, ch
pointer	sp, kwname
int	strdic(), strncmp(), strlen()
string	keywords "|ctime|history|limtime|maxpixval|minpixval|mtime|naxis\
|pixfile|pixtype|title|"

begin
	call smark (sp)
	call salloc (kwname, SZ_FNAME, TY_CHAR)

	# Look the string up in the dictionary of standard keywords.  Note that
	# the "i_" prefix is omitted in the dictionary.  The order of the
	# keywords in the dictionary must agree with the defined codes in the
	# header file.  A standard keyword is recognized with or without the
	# "i_" prefix.

	if (key[1] == 'i' && key[2] == '_')
	    ip = 3
	else
	    ip = 1

	# Check for a reference to one of the NAXIS keywords.
	if (key[ip] == 'n')
	    if (strncmp (key[ip], "naxis", 5) == 0) {
		ch = key[ip+5]
		if (ch == EOS || (IS_DIGIT(ch) && key[ip+6] == EOS)) {
		    call sfree (sp)
		    return (7)
		}
	    }

	# Look up keyword in dictionary.  Abbreviations are not permitted.
	index = strdic (key[ip], Memc[kwname], SZ_FNAME, keywords)
	if (index != 0)
	    if (strlen(key[ip]) != strlen(Memc[kwname]))
		index = 0

	call sfree (sp)
	return (index)
end
