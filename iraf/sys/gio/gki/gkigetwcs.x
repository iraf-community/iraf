# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<syserr.h>
include	<gki.h>

# GKI_GETWCS -- Retrieve the WCS from the CL process.  Used when opening a
# (non-metafile) device in append mode.
#
# BOI GKI_GETWCS L N
#
#        L(i)           3
#	 N(i)		number of words of WCS data to be read

procedure gki_getwcs (fd, wcs, len_wcs)

int	fd			# input/output file
pointer	wcs[ARB]		# array of WCS structures (output)
int	len_wcs			# number of ints (struct units) in array

size_t	sz_val
size_t	nchars
int	nwords
long	read()
short	gki[GKI_GETWCS_LEN]
data	gki[1] /BOI/, gki[2] /GKI_GETWCS/, gki[3] /GKI_GETWCS_LEN/
errchk	syserr, read, write, flush
include	"gki.com"

begin
	nwords = (len_wcs * SZ_POINTER / SZ_SHORT)
	gki[GKI_GETWCS_N] = nwords

	# Request CL to send SETWCS instruction back to us.  The directive
	# must be sent on the pseudofile control stream.

	sz_val = SZ_INT
	# arg2: incompatible pointer
	call write (PSIOCTRL, fd, sz_val)
	sz_val = GKI_GETWCS_LEN * SZ_SHORT
	call write (PSIOCTRL, gki, sz_val)
	call flush (PSIOCTRL)

	# Read the wcs data.  This is returned on the process CLIN channel 
	# by the CL.

	nchars = nwords * SZ_SHORT
	# arg2: incompatible pointer
	if (read (CLIN, wcs, nchars) != nchars)
	    call syserr (SYS_GGETWCS)
end
