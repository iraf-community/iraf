# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>

# CLGWRD -- Get a keyword parameter from the CL, and match it against
# a dictionary of legal keywords.  Any unambiguous abbreviation is
# accepted.  The full keyword string is returned in keyword, and the
# word index of the keyword in the dictionary is returned as the function
# value.

int procedure clgwrd (param, keyword, maxchar, dictionary)

char	param[ARB]		# CL parameter string
char	keyword[ARB]		# String matched in dictionary
int	maxchar			# Maximum size of str
char	dictionary[ARB]		# Dictionary string

pointer	sp, abbrev
int	kwindex, strdic()

begin
	call smark (sp)
	call salloc (abbrev, SZ_FNAME, TY_CHAR)

	call clgstr (param, Memc[abbrev], maxchar)
	kwindex = strdic (Memc[abbrev], keyword, maxchar, dictionary)

	if (kwindex <= 0)
	    call syserrs (SYS_CLGWRD, Memc[abbrev])

        call sfree (sp)
        return (kwindex)
end
