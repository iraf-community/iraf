# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<gki.h>

# GKI_SETWCS -- Copy the set of 16 WCS to the graphics controller in the CL
# process.  The WCS are transmitted as a binary array of WCS structures.
#
# BOI GKI_SETWCS L N WCS
#
#        L(i)            4 + N
#	 N(i)		 length of WCS field in words
#        WCS             binary copy of the 16 WCS structures, transmitted
#                            in a single call to WRITE

procedure gki_setwcs (fd, wcs, len_wcs)

int	fd			# output file
pointer	wcs[ARB]		# array of WCS structures
int	len_wcs			# number of ints (struct units) in array

size_t	sz_val
int	nshorts
short	gki[GKI_SETWCS_LEN]
data	gki[1] /BOI/, gki[2] /GKI_SETWCS/
include	"gki.com"

begin
	if (IS_FILE(fd)) {
	    nshorts = (len_wcs * SZ_POINTER) / SZ_SHORT
	    gki[GKI_SETWCS_L] = GKI_SETWCS_LEN + nshorts
	    gki[GKI_SETWCS_N] = nshorts

	    if (fd >= STDGRAPH && fd <= STDPLOT) {
		# Send a copy of the WCS information to the PSIO control
		# stream if the graphics output is a standard graphics stream.

		sz_val = SZ_INT
		# arg2: incompatible pointer
		call write (PSIOCTRL, fd, sz_val)
		sz_val = GKI_SETWCS_LEN * SZ_SHORT
		call write (PSIOCTRL, gki, sz_val)
		sz_val = nshorts * SZ_SHORT
		# arg2: incompatible pointer
		call write (PSIOCTRL, wcs, sz_val)
		call flush (PSIOCTRL)
	    }

	    sz_val = GKI_SETWCS_LEN * SZ_SHORT
	    call write (gk_fd[fd], gki, sz_val)
	    sz_val = nshorts * SZ_SHORT
	    # arg2: incompatible pointer
	    call write (gk_fd[fd], wcs, sz_val)
	}
end
