# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>

# STRDIC -- Search a dictionary string for a match with an input string.
# The input string may be an abbreviation of a dictionary entry, however,
# it is an error if the abbreviation is not unique.  The entries in the
# dictionary string are separated by a delimiter character which is the first
# character of the dictionary string.  The full name of the matched dictionary
# entry found is returned in out_str; the function value is the word index of
# the dictionary entry.  The output string may be the same as the input string.

int procedure strdic (in_str, out_str, maxchars, dict)

char	in_str[ARB]		# Input string, always lower case
char	out_str[ARB]		# Output string as found in dictionary
int	maxchars		# Maximum length of output string
char	dict[ARB]		# Dictionary string

char	ch, fch
int	start, len, ip, i, match, entry
int	strlen(), strncmp()

begin
	if (dict[1] == EOS)
	    return (0)

	for (i=1;  IS_WHITE (in_str[i]);  i=i+1)
	    ;

	start = i
	match = 0
	ip    = 2
	len   = strlen (in_str[start])
	fch   = in_str[start]

	# Search the dictionary string.  If the input string matches a
	# dictionary entry it is either an exact match (len = dictionary
	# entry length) or a legal abbreviation.  If an abbreviation
	# matches two entries it is ambiguous and an error.

	for (entry=1;  dict[ip] != EOS;  entry=entry+1) {
	    if (dict[ip] == fch) {
		if (strncmp (dict[ip], in_str[start], len) == 0) {
		    for (i=1;  i <= maxchars;  i=i+1) {
			ch = dict[ip+i-1]
			if ((ch == dict[1]) || (ch == EOS))
			    break
			out_str[i] = ch
		    }
		    out_str[i] = EOS

		    if ((dict[ip+len] == dict[1]) || (dict[ip+len] == EOS))
			return (entry)		# exact match
		    else {
			# If we already have a match and the new match is not
			# exact, then the abbreviation is ambiguous.

			if (match != 0)
			    return (0)
			else
			    match = entry
		    }
		}
	    }

	    repeat {
		ip = ip + 1
	    } until (dict[ip-1] == dict[1] || dict[ip] == EOS)
	}

        return (match)
end
