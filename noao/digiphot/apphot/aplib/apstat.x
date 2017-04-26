define	MAXERR1		500
define	MAXERR2		1000

# APSTATS -- Procedure to fetch an apphot string parameter.

procedure apstats (ap, param, str, maxch)

pointer	ap		# pointer to apphot structure
int	param		# parameter
char	str[ARB]	# string
int	maxch		# maximum number of characters

begin
	if (param <= MAXERR1)
	    call ap1stats (ap, param, str, maxch)
	else if (param <= MAXERR2)
	    call ap2stats (ap, param, str, maxch)
	else
	    call error (0, "APSTATS: Unknown apphot string parameter")
end


# APSTATI -- Procedure to set an integer apphot parameter.

int procedure apstati (ap, param)

pointer	ap		# pointer to apphot structure
int	param		# parameter

int	ap1stati(), ap2stati()

begin
	if (param <= MAXERR1)
	    return (ap1stati (ap, param))
	else if (param <= MAXERR2)
	    return (ap2stati (ap, param))
	else
	    call error (0, "APSTATI: Unknown apphot integer parameter")
end


# APSTATR -- Procedure to set a real apphot parameter.

real procedure apstatr (ap, param)

pointer	ap		# pointer to apphot structure
int	param		# parameter

real	ap1statr(), ap2statr()

begin
	if (param <= MAXERR1)
	    return (ap1statr (ap, param))
	else if (param <= MAXERR2)
	    return (ap2statr (ap, param))
	else
	    call error (0, "APSTATR: Unknown apphot real parameter")
end


# APSTATD -- Procedure to set a double apphot parameter.

double procedure apstatd (ap, param)

pointer	ap		# pointer to apphot structure
int	param		# parameter

double	ap1statd(), ap2statd()

begin
	if (param <= MAXERR1)
	    return (ap1statd (ap, param))
	else if (param <= MAXERR2)
	    return (ap2statd (ap, param))
	else
	    call error (0, "APSTATD: Unknown apphot double parameter")
end
