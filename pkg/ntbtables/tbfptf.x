# tbfptf -- change format from SPP to Fortran
# This is similar to tbbptf except that the output should be legal Fortran.
# The input and output may be the same string.
#
# Phil Hodge,  6-Jul-1995  Subroutine created

procedure tbfptf (sppfmt, ftnfmt, maxch)

char	sppfmt[ARB]	# i: print format in SPP style
char	ftnfmt[ARB]	# o: print format in Fortran style
int	maxch		# i: max size of ftnfmt
#--
pointer sp
pointer fmt		# scratch for Fortran format
pointer numpart		# copy of numerical portion of print format
char	sppcode		# SPP format code
char	fcode		# Fortran format code
int	fmtlen		# length of string sppfmt
int	index		# position of character in format string
int	w, d		# as in w.d
int	ip, ctoi()

string	sppchr	"fgdeHhMmbsxo"
string	ftnchr	"FGIEFFFFLAZO"

int	strlen(), stridx()

begin
	call smark (sp)
	call salloc (fmt, SZ_FNAME, TY_CHAR)
	call salloc (numpart, SZ_FNAME, TY_CHAR)

	fmtlen = strlen (sppfmt)

	# Copy numerical portion to numpart.  Ignore any minus sign (which
	# means left justify the value).
	if (sppfmt[2] == '-')
	    call strcpy (sppfmt[3], Memc[numpart], fmtlen-3)
	else
	    call strcpy (sppfmt[2], Memc[numpart], fmtlen-2)

	# Get fortran format code corresponding to spp format code.
	sppcode = sppfmt[fmtlen]
	index = stridx (sppcode, sppchr)
	if (index == 0) {
	    call sprintf (Memc[fmt], SZ_FNAME, "bad print format `%s'")
		call pargstr (sppfmt)
	    call error (1, Memc[fmt])
	} else {
	    fcode = ftnchr[index]
	}

	# Extract numerical parts (w.d).
	ip = 1
	if (ctoi (Memc[numpart], ip, w) < 0)
	    w = 0
	if (Memc[numpart+ip-1] == '.') {
	    ip = ip + 1
	    if (ctoi (Memc[numpart], ip, d) < 0)
		d = 0
	}

	# Construct Fortran format.
	if (sppfmt[fmtlen] == 'H' || sppfmt[fmtlen] == 'h') {
	    # Use F format instead of H.MSd, so increase w and d.
	    w = w + 4
	    d = d + 4
	    call sprintf (Memc[fmt], SZ_FNAME, "%c%d.%d")	# e.g. F12.5
		call pargc (fcode)
		call pargi (w)
		call pargi (d)
	} else if (sppfmt[fmtlen] == 'M' || sppfmt[fmtlen] == 'm') {
	    w = w + 2
	    d = d + 2
	    call sprintf (Memc[fmt], SZ_FNAME, "%c%d.%d")
		call pargc (fcode)
		call pargi (w)
		call pargi (d)
	} else if (Memc[numpart] == '0' &&
		(sppcode == 'd' || sppcode == 'o' || sppcode == 'x')) {
	    call sprintf (Memc[fmt], SZ_FNAME, "%c%d.%d")	# e.g. I4.4
		call pargc (fcode)
		call pargi (w)
		call pargi (w)
	} else {
	    # Append numerical portion from SPP format without modification.
	    Memc[fmt] = fcode
	    Memc[fmt+1] = EOS
	    call strcat (Memc[numpart], Memc[fmt], SZ_FNAME)
	}

	call strcpy (Memc[fmt], ftnfmt, maxch)

	call sfree (sp)
end
