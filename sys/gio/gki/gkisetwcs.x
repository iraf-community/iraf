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
int	wcs[ARB]		# array of WCS structures
int	len_wcs			# number of ints (struct units) in array

int	nshorts
short	gki[GKI_SETWCS_LEN]
data	gki[1] /BOI/, gki[2] /GKI_SETWCS/
include	"gki.com"

begin
	if (IS_FILE(fd)) {
	    nshorts = (len_wcs * SZ_INT) / SZ_SHORT
	    gki[GKI_SETWCS_L] = GKI_SETWCS_LEN + nshorts
	    gki[GKI_SETWCS_N] = nshorts

	    if (fd >= STDGRAPH && fd <= STDPLOT) {
		# Send a copy of the WCS information to the PSIO control
		# stream if the graphics output is a standard graphics stream.

		call write (PSIOCTRL, fd, SZ_INT32)
		call write (PSIOCTRL, gki, GKI_SETWCS_LEN * SZ_SHORT)
		call write (PSIOCTRL, wcs, nshorts * SZ_SHORT)
		call flush (PSIOCTRL)
	    }

	    call write (gk_fd[fd], gki, GKI_SETWCS_LEN * SZ_SHORT)
	    call write (gk_fd[fd], wcs, nshorts * SZ_SHORT)
	}
end
