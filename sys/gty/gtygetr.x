# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>

# GTYGETR -- Get a real valued capability.  If the capability is not
# found for the device, or cannot be interpreted as a number, zero is
# returned.  Real valued capabilities have the format ":xx#num:".

real procedure gtygetr (tty, cap)

pointer	tty			# tty descriptor
char	cap[ARB]		# two character capability name

char	numstr[MAX_DIGITS]
int	np, op
pointer	ip
double	dval
int	gty_find_capability(), ctod()

begin
	if (gty_find_capability (tty, cap, ip) == NO)
	    return (0.0)
	else if (Memc[ip] != '#')
	    return (0.0)
	else {
	    # Extract the number into numstr.  Cannot convert in place in
	    # the table because the ":" delimiter will by interpreted by
	    # ctod as for a sexagesimal number.
	    op = 1
	    for (ip=ip+1;  op <= MAX_DIGITS && Memc[ip] != ':';  ip=ip+1) {
		numstr[op] = Memc[ip]
		op = op + 1
	    }
	    numstr[op] = EOS
	    np = 1
	    if (ctod (numstr, np, dval) == 0)
		return (0.0)
	    else
		return (dval)
	}
end
