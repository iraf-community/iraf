include <tbset.h>
include "../lib/apseldef.h"

# DP_PFMERGE -- Read the input photometry file, extract the fields ID,
# XCENTER, YCENTER, MAG, and MSKY from each input record and add these
# records to the output file.

define	NCOLUMN	5

procedure dp_pfmerge (infd, outfd, in_text, out_text, first_file)

int	infd		# the input file descriptor
int	outfd		# the output file descriptor
int	in_text		# input text file ?
int	out_text	# output text file ?
int	first_file	# first file ?

int	nrow, instar, outstar, id
pointer	sp, indices, fields, ocolpoint, key
real	x, y, mag, sky
int	tbpsta(), dp_rrphot()

begin
	# Allocate some memory.
	call smark (sp)
	call salloc (indices, NAPPAR, TY_INT)
	call salloc (fields, SZ_LINE, TY_CHAR)
	call salloc (ocolpoint, NCOLUMN, TY_POINTER)

	# Initialize the output file.
	if (first_file == YES) {
	    if (out_text == YES) {
		call seek (infd, BOF)
		call dp_apheader (infd, outfd)
		call dp_xpbanner (outfd)
	    } else {
		call dp_tpdefcol (outfd, Memi[ocolpoint])
		call tbhcal (infd, outfd)
	    }
	    outstar = 0
	}

	# Initialize the input file
	if (in_text == YES) {
	    call pt_kyinit (key)
	    Memi[indices] = DP_PAPID
	    Memi[indices+1] = DP_PAPXCEN
	    Memi[indices+2] = DP_PAPYCEN
	    Memi[indices+3] = DP_PAPMAG1
	    Memi[indices+4] = DP_PAPSKY
	    call dp_gappsf (Memi[indices], Memc[fields], NAPRESULT)
	    nrow = 0
	} else {
	    call dp_tpkinit (infd, Memi[indices])
	    nrow = tbpsta (infd, TBL_NROWS)
	}

	# Loop over the stars.
	instar = 0
	repeat {
	    
	    # Read the input record.
	    if (dp_rrphot (infd, key, Memc[fields], Memi[indices], id,
	        x, y, sky, mag, instar, nrow) == EOF)
		break

	    # Write the output record.
	    outstar = outstar + 1
	    if (out_text == YES)
		call dp_xpselmer (outfd, id, x, y, mag, sky)
	    else
		call dp_tpselmer (outfd, id, x, y, mag, sky, Memi[ocolpoint],
		    outstar)
	}


	if (in_text == YES)
	    call pt_kyfree (key)

	call sfree (sp)
end
