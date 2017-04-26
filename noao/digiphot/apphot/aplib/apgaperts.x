include <lexnum.h>
include <ctype.h>

# AP_GETAPERTS -- Procedure to extract real aperture values from a string

int procedure ap_getaperts (str, aperts, max_naperts)

char	str[ARB]		# string
real	aperts[ARB]		# number of apertures
int	max_naperts		# maximum number of apertures

int	fd, naperts
int	open(), ap_rdaperts(), ap_decaperts()
errchk	open(), close()

begin
	naperts = 0

	iferr {
	    fd = open (str, READ_ONLY, TEXT_FILE)
	    naperts = ap_rdaperts (fd, aperts, max_naperts)
	    call close (fd)
	} then {
	    naperts = ap_decaperts (str, aperts, max_naperts)
	}

	return (naperts)
end


# AP_RDAPERTS -- Procedure to read out the apertures listed one per line
# from a file.

int procedure ap_rdaperts (fd, aperts, max_naperts)

int	fd		# aperture list file descriptor
real	aperts[ARB]	# list of apertures
int	max_naperts	# maximum number of apertures

int	naperts
pointer	sp, line
int	getline(), ap_decaperts()

begin
	call smark (sp)
	call salloc (line, SZ_LINE, TY_CHAR)

	naperts = 0
	while (getline (fd, Memc[line]) != EOF && naperts < max_naperts) {
	    naperts = naperts + ap_decaperts (Memc[line], aperts[1+naperts],
	        max_naperts - naperts)
	}

	call sfree (sp)

	return (naperts)
end


# AP_DECAPERTS -- Procedure to decode the aperture string.

int procedure ap_decaperts (str, aperts, max_naperts)

char	str[ARB]		# aperture string
real	aperts[ARB]		# aperture array
int	max_naperts		# maximum number of apertures

char	outstr[SZ_LINE]
int	naperts, ip, op, ndecode, nap
real	apstart, apend, apstep
bool	fp_equalr()
int	gctor()

begin
	naperts = 0

	for (ip = 1; str[ip] != EOS && naperts < max_naperts;) {

	    apstart = 0.0
	    apend = 0.0
	    apstep = 0.0
	    ndecode = 0

	    # Skip past white space and commas.
	    while (IS_WHITE(str[ip]))
		ip = ip + 1
	    if (str[ip] == ',')
		ip = ip + 1

	    # Get the number.
	    op = 1
	    while (IS_DIGIT(str[ip]) || str[ip] == '.') {
		outstr[op] = str[ip]
		ip = ip + 1
		op = op + 1
	    }
	    outstr[op] = EOS

	    # Decode the starting aperture.
	    op = 1
	    if (gctor (outstr, op, apstart) > 0) {
	        apend = apstart
	        ndecode = 1
	    } else
		apstart = 0.0

	    # Skip past white space and commas.
	    while (IS_WHITE(str[ip]))
		ip = ip + 1
	    if (str[ip] == ',')
		ip = ip + 1

	    # Get the ending aperture
	    if (str[ip] == ':') {
		ip = ip + 1

		# Get the ending aperture.
		op = 1
		while (IS_DIGIT(str[ip]) || str[ip] == '.') {
		    outstr[op] = str[ip]
		    ip = ip + 1
		    op = op + 1
		}
		outstr[op] = EOS

	        # Decode the ending aperture.
	        op = 1
	        if (gctor (outstr, op, apend) > 0) {
	            ndecode = 2
	            apstep = apend - apstart
		}
	     }

	    # Skip past white space and commas.
	    while (IS_WHITE(str[ip]))
		ip = ip + 1
	    if (str[ip] == ',')
		ip = ip + 1

	    # Get the step size.
	    if (str[ip] == ':') {
		ip = ip + 1

		# Get the step size.
		op = 1
		while (IS_DIGIT(str[ip]) || str[ip] == '.') {
		    outstr[op] = str[ip]
		    ip = ip + 1
		    op = op + 1
		}
		outstr[op] = EOS

		# Decode the step size.
		op = 1
		if (gctor (outstr, op, apstep) > 0) {
		    if (fp_equalr (apstep, 0.0))
			apstep = apend - apstart
		    else
			ndecode = (apend - apstart) / apstep + 1
		    if (ndecode < 0) {
			ndecode = -ndecode
			apstep = - apstep
		    }
		}
	    }

	    # Negative apertures are not permitted.
	    if (apstart <= 0.0 || apend <= 0.0)
		break

	    # Fill in the apertures.
	    if (ndecode == 0) {
		;
	    } else if (ndecode == 1) {
		naperts = naperts + 1
		aperts[naperts] = apstart
	    } else if (ndecode == 2) {
		naperts = naperts + 1
		aperts[naperts] = apstart
		if (naperts >= max_naperts)
		    break
		naperts = naperts + 1
		aperts[naperts] = apend
	    } else {
		for (nap = 1; nap <= ndecode && naperts < max_naperts;
		    nap = nap + 1) {
		    naperts = naperts + 1
		    aperts[naperts] = apstart + (nap - 1) * apstep
		}
	    }
	}

	return (naperts)
end


# GCTOR -- Procedure to convert a character variable to a real number.
# This routine is just an interface routine to the IRAF procedure gctod.

int procedure gctor (str, ip, rval)

char	str[ARB]	# string to be converted
int	ip		# pointer to the string
real	rval		# real value

double	dval
int	nchars
int	gctod()

begin
	nchars = gctod (str, ip, dval)
	rval = dval
	return (nchars)
end
