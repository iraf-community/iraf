# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>
include	"help.h"

# GET_OPTION -- Map the possibly abbreviated option string into an integer
# option code.  It is an error if the abbreviation is not unique.

define	EQUAL		0

int procedure get_option (opstr)

char	opstr[ARB]		# option string, always lower case
int	keycode[MAX_OPTIONS]
int	oplen, key, ip, match
int	strlen(), strncmp()

string	keywords "help|sources|sysdoc|alldoc|files|summary|directory|references"
data	keycode /O_HELP,O_SOURCE,O_SYSDOC,O_ALLDOC,O_FILES,O_SUMMARY,O_DIR,
		 O_REFERENCES/

begin
	oplen = strlen (opstr)
	ip = 1
	match = 0

	# Search the keyword table.  If the option string matches a keyword,
	# it is either an exact match (oplen = keyword length) or a legal
	# abbreviation.  If an abbreviation matches two keywords it is
	# ambiguous and an error.

	for (key=1;  keywords[ip] != EOS;  key=key+1) {
	    if (strncmp (keywords[ip], opstr, oplen) == EQUAL) {
		if (keywords[ip+oplen] == '|')
		    return (keycode[key])		# exact match
		else {
		    if (match != 0)
			call error (1, "ambiguous help option abbreviation")
		    else
			match = key
		}
	    }
	    repeat {
		ip = ip + 1
	    } until (keywords[ip-1] == '|' || keywords[ip] == EOS)
	}

	if (match == 0)
	    call error (2, "unknown help option")
	else
	    return (keycode[match])
end
