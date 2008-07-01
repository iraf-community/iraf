# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<gki.h>
include	"sgk.h"

define	LEN_MCBUF	3000


# SGIDECODE -- Decode an SGI metacode file, printing the decoded metacode
# instructions on the standard output.

procedure t_sgidecode()

pointer	sp, fname, mcbuf, ip, itop, list
int	fd, verbose, gkiunits
long	nwords

size_t	sz_val
bool	clgetb()
int	clgfil(), clplen(), open(), btoi()
long	miireads()
pointer	clpopni()

begin
	call smark (sp)
	sz_val = SZ_FNAME
	call salloc (fname, sz_val, TY_CHAR)
	sz_val = LEN_MCBUF
	call salloc (mcbuf, sz_val, TY_SHORT)

	# Open list of metafiles to be decoded.
	list = clpopni ("input")

	if (clgetb ("generic")) {
	    verbose = NO
	    gkiunits = NO
	} else {
	    verbose = btoi (clgetb ("verbose"))
	    gkiunits = btoi (clgetb ("gkiunits"))
	}

	# Process a list of metacode files, writing the decoded metacode
	# instructions on the standard output.

	while (clgfil (list, Memc[fname], SZ_FNAME) != EOF) {
	    # Print header if new file.
	    if (clplen (list) > 1) {
		call printf ("\n# METAFILE `%s':\n")
		    call pargstr (Memc[fname])
	    }

	    # Open input file.
	    iferr (fd = open (Memc[fname], READ_ONLY, BINARY_FILE)) {
		call erract (EA_WARN)
		next
	    }

	    # Process the metacode.

	    itop = mcbuf
	    ip   = mcbuf

	    repeat {
		if (ip >= itop) {
		    # Refill buffer.
		    sz_val = LEN_MCBUF
		    nwords = miireads (fd, Mems[mcbuf], sz_val)
		    if (nwords == EOF)
			break
		    itop = mcbuf + nwords
		    ip = mcbuf
		}
		
		switch (Mems[ip]) {
		case SGK_FRAME:
		    call printf ("new_frame\n")
		case SGK_MOVE:
		    if (gkiunits == YES) {
			call printf ("move (%d, %d)\n")
			    call pargs (Mems[ip+1])
			    call pargs (Mems[ip+2])
		    } else {
			call printf ("move (%0.5f, %0.5f)\n")
			    call pargr (real(Mems[ip+1]) / real(GKI_MAXNDC))
			    call pargr (real(Mems[ip+2]) / real(GKI_MAXNDC))
		    }
		case SGK_DRAW:
		    if (gkiunits == YES) {
			call printf ("draw (%d, %d)\n")
			    call pargs (Mems[ip+1])
			    call pargs (Mems[ip+2])
		    } else {
			call printf ("draw (%0.5f, %0.5f)\n")
			    call pargr (real(Mems[ip+1]) / real(GKI_MAXNDC))
			    call pargr (real(Mems[ip+2]) / real(GKI_MAXNDC))
		    }
		case SGK_SETLW:
		    call printf ("set_linewidth (%d)\n")
			call pargs (Mems[ip+1])
		default:
		    call printf ("unknown instruction\n")
		}

		ip = ip + SGK_LENMCI
	    }

	    call close (fd)
	}

	call clpcls (list)
	call sfree (sp)
end
