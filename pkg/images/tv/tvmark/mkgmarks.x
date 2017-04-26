include <lexnum.h>
include <ctype.h>

# MK_GMARKS -- Procedure to extract mark values from a string

int procedure mk_gmarks (str, marks, max_nmarks)

char	str[ARB]		# string
real	marks[ARB]		# number of marks
int	max_nmarks		# maximum number of marks

int	fd, nmarks
int	open(), mk_rdmarks(), mk_decmarks()
errchk	open(), close()

begin
	nmarks = 0

	iferr {
	    fd = open (str, READ_ONLY, TEXT_FILE)
	    nmarks = mk_rdmarks (fd, marks, max_nmarks)
	    call close (fd)
	} then {
	    nmarks = mk_decmarks (str, marks, max_nmarks)
	}

	return (nmarks)
end


# MK_RDMARKS -- Procedure to read out the marks listed one per line
# from a file.

int procedure mk_rdmarks (fd, marks, max_nmarks)

int	fd		# aperture list file descriptor
real	marks[ARB]	# list of marks
int	max_nmarks	# maximum number of apertures

int	nmarks
pointer	sp, line
int	getline(), mk_decmarks()

begin
	call smark (sp)
	call salloc (line, SZ_LINE, TY_CHAR)

	nmarks = 0
	while (getline (fd, Memc[line]) != EOF && nmarks < max_nmarks) {
	    nmarks = nmarks + mk_decmarks (Memc[line], marks[1+nmarks],
	        max_nmarks - nmarks)
	}

	call sfree (sp)

	return (nmarks)
end


# MK_DECAPERTS -- Procedure to decode the mark string.

int procedure mk_decmarks (str, marks, max_nmarks)

char	str[ARB]		# aperture string
real	marks[ARB]		# aperture array
int	max_nmarks		# maximum number of apertures

char	outstr[SZ_LINE]
int	nmarks, ip, op, ndecode, nmk
real	mkstart, mkend, mkstep
bool	fp_equalr()
int	gctor()

begin
	nmarks = 0

	for (ip = 1; str[ip] != EOS && nmarks < max_nmarks;) {

	    mkstart = 0.0
	    mkend = 0.0
	    mkstep = 0.0
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
	    if (gctor (outstr, op, mkstart) > 0) {
	        mkend = mkstart
	        ndecode = 1
	    } else
		mkstart = 0.0

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
	        if (gctor (outstr, op, mkend) > 0) {
	            ndecode = 2
	            mkstep = mkend - mkstart
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
		if (gctor (outstr, op, mkstep) > 0) {
		    if (fp_equalr (mkstep, 0.0))
			mkstep = mkend - mkstart
		    else
			ndecode = (mkend - mkstart) / mkstep + 1
		    if (ndecode < 0) {
			ndecode = -ndecode
			mkstep = - mkstep
		    }
		}
	    }

	    # Negative apertures are not permitted.
	    if (mkstart <= 0.0 || mkend <= 0.0)
		break

	    # Fill in the apertures.
	    if (ndecode == 0) {
		;
	    } else if (ndecode == 1) {
		nmarks = nmarks + 1
		marks[nmarks] = mkstart
	    } else if (ndecode == 2) {
		nmarks = nmarks + 1
		marks[nmarks] = mkstart
		if (nmarks >= max_nmarks)
		    break
		nmarks = nmarks + 1
		marks[nmarks] = mkend
	    } else {
		for (nmk = 1; nmk <= ndecode && nmarks < max_nmarks;
		    nmk = nmk + 1) {
		    nmarks = nmarks + 1
		    marks[nmarks] = mkstart + (nmk - 1) * mkstep
		}
	    }
	}

	return (nmarks)
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
