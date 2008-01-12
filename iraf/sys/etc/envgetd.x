# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<mach.h>

# ENVGETD -- Fetch an environment variable and try to interpret its value
# as a double.  Abort if variable is not found or cannot be converted to
# a number.

double procedure envgetd (varname)

char	varname[ARB]

int	ip
double	dval
char	val[MAX_DIGITS]
int	ctod(), envfind()
errchk	envfind, syserrs

begin
	ip = 1
	if (envfind (varname, val, MAX_DIGITS) > 0)
	    if (ctod (val, ip, dval) > 0)
		return (dval)

	call syserrs (SYS_ENVNNUM, varname)
end
