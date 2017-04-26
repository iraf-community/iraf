# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# STRSEARCH -- Search a string for a substring.  This is the simplest and
# fastest member of the pattern matching family.  A significant increase in
# efficiency will result if this procedure is used to search for substrings
# that do not use any metacharacters.

int procedure strsearch (str, patstr)

char	str[ARB]		# string to be searched
char	patstr[ARB]		# substring to search for

int	first_char, ch
int	ip, patlen
bool	strse1()

begin
	# The null pattern matches any string.
	if (patstr[1] == EOS)
	    return (1)

	first_char = patstr[1]

	do ip = 1, ARB {
	    ch = str[ip]
	    if (ch == EOS)
		break
	    if (ch == first_char)
		if (strse1 (str[ip], patstr, patlen))
		    return (ip + patlen)
	}

	return (0)
end


# STRSE1 -- Internal routine which compares a substring of the first string
# with the pattern string.  STREQ cannot be used because it does not give a
# match unless the two strings have the same length.

bool procedure strse1 (str, patstr, patlen)

char	str[ARB]
char	patstr[ARB]
int	patlen
int	ip

begin
	do ip = 1, ARB
	    if (patstr[ip] == EOS || str[ip] != patstr[ip])
		break

	patlen = ip - 1
	return (patstr[ip] == EOS)
end
