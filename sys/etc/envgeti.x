# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<mach.h>

# ENVGETI -- Fetch an environment variable and try to interpret its value
# as an integer.  Abort if variable is not found or cannot be converted to
# a number.

int procedure envgeti (varname)

char	varname[ARB]

int	ival, ip
char	val[MAX_DIGITS]
int	ctoi(), envfind()
errchk	envfind, syserrs

begin
	ip = 1
	if (envfind (varname, val, MAX_DIGITS) > 0)
	    if (ctoi (val, ip, ival) > 0)
		return (ival)

	call syserrs (SYS_ENVNNUM, varname)
end
