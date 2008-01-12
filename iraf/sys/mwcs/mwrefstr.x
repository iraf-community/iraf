# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "mwcs.h"

# MW_REFSTR -- Search the string buffer for the named string and return the
# string offset if found, otherwise enter the new string and return its
# offset.  This is used to avoid storing the same string many times, but
# use of this technique means that string data cannot be modified once
# entered.

int procedure mw_refstr (mw, str)

pointer	mw			#I pointer to MWCS descriptor
char	str[ARB]		#I string to be referenced or entered

bool	match
pointer	sbuf, btop, ip
int	nchars, off, ch, i
int	strlen(), mw_allocs()
errchk	mw_allocs

begin
	sbuf = MI_SBUF(mw)
	btop = sbuf + MI_SBUFLEN(mw)
	nchars = strlen (str)

	# Search the string buffer for the given string.
	match = false
	if (sbuf != NULL)
	    for (ip=sbuf;  !match && ip < btop;  ) {
		match = true
		do i = 1, btop-ip {
		    ch = Memc[ip+i-1]
		    if (i <= nchars)
			if (ch != str[i])
			    match = false
		    if (ch == EOS) {
			if (!match)
			    ip = ip + i
			break
		    }
		}
		if (ch != EOS)
		    break
	    }

	# Add the string if not found.
	if (!match) {
	    off = mw_allocs (mw, nchars)
	    call strcpy (str, S(mw,off), nchars)
	} else
	    off = ip - sbuf + 1

	return (off)
end
