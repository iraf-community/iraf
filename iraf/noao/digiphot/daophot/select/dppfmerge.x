include <tbset.h>
include "../lib/apseldef.h"

# DP_PFMERGE -- Read the input photometry file, extract the fields ID,
# XCENTER, YCENTER, MAG, and MSKY from each input record and add these
# records to the output file.

define	NCOLUMN	5

procedure dp_pfmerge (infd, outfd, in_text, out_text, first_file)

pointer	infd		# the input file descriptor
pointer	outfd		# the output file descriptor
int	in_text		# input text file ?
int	out_text	# output text file ?
int	first_file	# first file ?

size_t	sz_val
long	nrow, instar, outstar, l_val
int	id, i_infd, i_outfd
pointer	sp, fields, ocolpoint, key, indices, p_indices
real	x, y, mag, sky
int	dp_rrphot()
long	tbpstl()

begin
	i_infd = infd
	i_outfd = outfd

	# Allocate some memory.
	call smark (sp)
	sz_val = NAPPAR
	call salloc (indices, sz_val, TY_INT)
	call salloc (p_indices, sz_val, TY_POINTER)
	sz_val = SZ_LINE
	call salloc (fields, sz_val, TY_CHAR)
	sz_val = NCOLUMN
	call salloc (ocolpoint, sz_val, TY_POINTER)

	# Initialize the output file.
	if (first_file == YES) {
	    if (out_text == YES) {
		l_val = BOF
		call seek (i_infd, l_val)
		call dp_apheader (i_infd, i_outfd)
		call dp_xpbanner (i_outfd)
	    } else {
		call dp_tpdefcol (outfd, Memp[ocolpoint])
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
	    call dp_tpkinit (infd, Memp[p_indices])
	    nrow = tbpstl (infd, TBL_NROWS)
	}

	# Loop over the stars.
	instar = 0
	repeat {
	    
	    # Read the input record.
	    if (dp_rrphot (infd, key, Memc[fields],
			   Memi[indices], Memp[p_indices], id,
			   x, y, sky, mag, instar, nrow) == EOF)
		break

	    # Write the output record.
	    outstar = outstar + 1
	    if (out_text == YES)
		call dp_xpselmer (i_outfd, id, x, y, mag, sky)
	    else
		call dp_tpselmer (outfd, id, x, y, mag, sky, Memp[ocolpoint],
		    outstar)
	}


	if (in_text == YES)
	    call pt_kyfree (key)

	call sfree (sp)
end
