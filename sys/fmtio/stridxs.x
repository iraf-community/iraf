# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

define	BIGSET		10
define	SZ_ASCII	128

# STRIDXS -- Return the index of the first occurrence of any of a set of
# characters in a string.

int procedure stridxs (set, str)

char	set[ARB]		# set of characters to be searched for
char	str[ARB]		# string to be searched

int	setlen, ip, i
char	ch, lut[SZ_ASCII]
int	strlen()

begin
	setlen = strlen (set)

	if (setlen > BIGSET) {
	    # Encode the set in a lookup table.
	    call aclrc (lut, SZ_ASCII)
	    do i = 1, setlen
		lut[set[i]] = 1

	    # Search the string.
	    for (ip=1;  str[ip] != EOS;  ip=ip+1)
		if (lut[str[ip]] != 0)
		    return (ip)

	} else {
	    # Set is too small to be worth using a lookup table.
	    for (ip=1;  str[ip] != EOS;  ip=ip+1) {
		ch = str[ip]
		do i = 1, setlen
		    if (ch == set[i])
			return (ip)
	    }
	}
	
	return (0)
end
