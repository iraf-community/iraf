# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<knet.h>
include	<fset.h>
include	"environ.h"

# ENVGETS -- Search the environment list for the named environment variable
# and return the string value if found.  If not found and the process input
# is a terminal (the process is being run interactively in debug mode), generate
# a query on the terminal, read the value of the environment variable, enter
# it into the environment table, and return the value to the caller.

int procedure envgets (key, value, maxch)

char	key[ARB]		# environment variable name
char	value[maxch]		# string value (output)
int	maxch

char	buf[SZ_FNAME]
int	nchars, ttydriver, junk, in, out, ip
int	gstrcpy(), envfind(), fstati(), strlen(), envputs()
extern	zgetty()

begin
	# Search the environment list first.
	nchars = envfind (key, value, maxch)
	if (nchars >= 0)
	    return (nchars)

	# Key not found.  If the process input CLIN is a terminal, query the
	# user for the value of the environment variable.  Only low level
	# calls are used in the query to avoid the possibity of recursion.

	call zlocpr (zgetty, ttydriver)
	iferr {
	    out = fstati (CLOUT, F_CHANNEL)
	    in  = fstati (CLIN,  F_CHANNEL)
	} then
	    return (0)

	if (fstati (CLIN, F_DEVICE) == ttydriver) {
	    # Issue prompt, format "env.key: "
	    call zputty (out, "env.", 4, junk)
	    call zputty (out, key, strlen(key), junk)
	    call zputty (out, ": ", 2, junk)
	    call zflsty (out, junk)

	    # Get value and enter in envlist, excluding the trailing newline.

	    call zgetty (in, buf, SZ_FNAME, nchars)
	    if (nchars <= 0)
		return (0)
	    for (ip=1;  buf[ip] != '\n' && ip <= nchars;  ip=ip+1)
		;
	    buf[ip] = EOS
	    junk = envputs (key, buf)

	    return (gstrcpy (buf, value, maxch))

	} else
	    return (0)
end
