# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<gki.h>

# GKI_ESCAPE -- Pass a device dependent instruction on to the kernel.
#
# BOI GKI_ESCAPE L FN N DC
#
#        L(i)            5 + N
#        FN(i)           escape function code
#        N(i)            number of escape data words
#        DC(i)           escape data words

procedure gki_escape (fd, fn, instruction, nwords)

int	fd			# output file
int	fn			# function code
short	instruction[ARB]	# instruction sequence of unknown format
int	nwords			# number of shorts in instruction

int	epa
short	gki[GKI_ESCAPE_LEN]
data	gki[1] /BOI/, gki[2] /GKI_ESCAPE/
include	"gki.com"

begin
	if (IS_INLINE(fd)) {
	    epa = gk_dd[GKI_ESCAPE]
	    if (epa != 0)
		call zcall3 (epa, fn, instruction, nwords)
	} else {
	    gki[GKI_ESCAPE_L]  = GKI_ESCAPE_LEN + nwords
	    gki[GKI_ESCAPE_N]  = nwords
	    gki[GKI_ESCAPE_FN] = fn

	    call write (gk_fd[fd], gki, GKI_ESCAPE_LEN * SZ_SHORT)
	    call write (gk_fd[fd], instruction, nwords * SZ_SHORT)
	}
end
