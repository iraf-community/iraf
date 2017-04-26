define	MAXERR1		500
define	MAXERR2		1000


# APSETS -- Procedure to set an apphot string parameter.

procedure apsets (ap, param, str)

pointer	ap		# pointer to apphot structure
int	param		# parameter
char	str[ARB]	# string parameter

begin
	if (param <= MAXERR1)
	    call ap1sets (ap, param, str)
	else if (param <= MAXERR2)
	    call ap2sets (ap, param, str)
end


# APSETI -- Procedure to set an integer apphot parameter.

procedure apseti (ap, param, ival)

pointer	ap		# pointer to apphot structure
int	param		# parameter
int	ival		# integer value

begin
	if (param <= MAXERR1)
	    call ap1seti (ap, param, ival)
	else if (param <= MAXERR2)
	    call ap2seti (ap, param, ival)
	else
	    call error (0, "Unknown APPHOT integer parameter")
end


# APSETR -- Procedure to set a real apphot parameter.

procedure apsetr (ap, param, rval)

pointer	ap		# pointer to apphot structure
int	param		# parameter
real	rval		# real value

begin
	if (param <= MAXERR1)
	    call ap1setr (ap, param, rval)
	else if (param <= MAXERR2)
	    call ap2setr (ap, param, rval)
	else
	    call error (0, "Unknown APPHOT real parameter")
end


# APSETD -- Procedure to set a double apphot parameter.

procedure apsetd (ap, param, dval)

pointer	ap		# pointer to apphot structure
int	param		# parameter
double	dval		# double value

begin
	if (param <= MAXERR1)
	    call ap1setd (ap, param, dval)
	else if (param <= MAXERR2)
	    call ap2setd (ap, param, dval)
	else
	    call error (0, "Unknown APPHOT double parameter")
end
