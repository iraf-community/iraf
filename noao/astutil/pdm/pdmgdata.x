include <mach.h>
include <ctype.h>
include <error.h>
include "pdm.h"

define	SZ_BUF	100

# PDM_GDATA -- Get Data from the input files.

int procedure pdm_gdata (pdmp, infile)

pointer	pdmp			# pointer to PDM data structure
char	infile[SZ_LINE]		# input data file name

int	fntopnb(), list, clgfil()
int	n, ncols, lineno, buflen
int	open(), getline(), nscan()
int	fd
pointer	nextfile
pointer	lbuf, ip, sp
errchk	realloc, fntopnb, open

begin
	# Get a line buffer.
	call smark (sp)
	call salloc (lbuf, SZ_LINE, TY_CHAR)
	call salloc (nextfile, SZ_LINE, TY_CHAR)

	# Open the input file as a list of files.
	list = fntopnb (infile, 0)

	# Initialize some variables.
	n = 0
	ncols = 0
	lineno = 0

	# For each input file in the list, read the data.
	while (clgfil (list, Memc[nextfile], SZ_FNAME) != EOF) {

	    # Open this input file.
	    fd = open (Memc[nextfile], READ_ONLY, TEXT_FILE)

	    # Read in the data from this file.
	    while (getline (fd, Memc[lbuf]) != EOF) {
	        # Skip white space and blank lines.
	        lineno = lineno + 1
	        for (ip = lbuf; IS_WHITE(Memc[ip]); ip = ip + 1)
		    ;
	        if (Memc[ip] == '\n' || Memc[ip] == EOS)
		    next

	        if (n == 0) {
		    buflen = SZ_BUF
		    iferr {
		        call calloc (PDM_XP(pdmp), buflen, TY_DOUBLE)
		        call calloc (PDM_DYP(pdmp), buflen, TY_DOUBLE)
		        call calloc (PDM_ODYP(pdmp), buflen, TY_DOUBLE)
		        call calloc (PDM_INUSEP(pdmp), buflen, TY_INT)
		        call calloc (PDM_ERRP(pdmp), buflen, TY_REAL)
		    } then
		        call erract (EA_FATAL)
	        } else if (n + 1 > buflen) {
		    buflen = buflen + SZ_BUF
		    call realloc (PDM_XP(pdmp), buflen, TY_DOUBLE)
		    call realloc (PDM_DYP(pdmp), buflen, TY_DOUBLE)
		    call realloc (PDM_ODYP(pdmp), buflen, TY_DOUBLE)
		    call realloc (PDM_INUSEP(pdmp), buflen, TY_INT)
		    call realloc (PDM_ERRP(pdmp), buflen, TY_REAL)
	        }

		# Read data from the file, put it in the data structure.
	        call sscan (Memc[ip])
	        call gargd (PDM_X(pdmp,n+1))
	        call gargd (PDM_ODY(pdmp,n+1))
	        call gargr (PDM_ERR(pdmp,n+1))
	        PDM_INUSE(pdmp,n+1) = 1
	        PDM_DY(pdmp,n+1) = PDM_ODY(pdmp,n+1)

	        # If this is line one, then determine the number of columns.
	        if (ncols == 0 && nscan() > 0)
		    ncols = nscan()

	        # Check this line against the number of columns and do the
	        # appropriate thing.

	        switch (nscan()) {
	        case 0:
		    call printf ("no args; %s, line %d: %s\n")
		        call pargstr (Memc[nextfile])
		        call pargi (lineno)
		        call pargstr (Memc[lbuf])
		    next
	        case 1:
		    if (ncols >= 2) {
		        call eprintf ("only one arg; %s, line %d: %s\n")
		            call pargstr (Memc[nextfile])
		            call pargi (lineno)
		            call pargstr (Memc[lbuf])
		        next
		    } else {
		        PDM_ODY(pdmp,n+1) = PDM_X(pdmp,n+1)
		        PDM_DY(pdmp,n+1) = PDM_X(pdmp,n+1)
		        PDM_X(pdmp,n+1) = n + 1.0d+0
			PDM_ERR(pdmp,n+1) = 0.0
		    }
		case 2:
		    if (ncols == 3) {
			call eprintf ("only two args; %s, line %d: %s\n")
			    call pargstr (Memc[nextfile])
			    call pargi (lineno)
			    call pargstr (Memc[lbuf])
			next
		    } else {
		        PDM_ODY(pdmp,n+1) = PDM_ODY(pdmp,n+1)
		        PDM_DY(pdmp,n+1) = PDM_ODY(pdmp,n+1)
		        PDM_X(pdmp,n+1) = PDM_X(pdmp,n+1)
			PDM_ERR(pdmp,n+1) = 0.0
		    }

	        }

	        n = n + 1
	    }
	    call close (fd)
	}

	call realloc (PDM_XP(pdmp), n, TY_DOUBLE)
	call realloc (PDM_DYP(pdmp), n, TY_DOUBLE)
	call realloc (PDM_ODYP(pdmp), n, TY_DOUBLE)
	call realloc (PDM_INUSEP(pdmp), n, TY_INT)
	call realloc (PDM_ERRP(pdmp), n, TY_REAL)

	call fntclsb (list)
	call sfree (sp)
	return (n)
end
