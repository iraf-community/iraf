# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<gki.h>

# GKI_EOF -- Signal end of file on a metacode stream.
#
# BOI GKI_EOF 0
#    
#        L(i)            set to the constant 3 (no data fields)

procedure gki_eof (fd)

int	fd			# output file

short	gki[GKI_EOF_LEN]
data	gki[1] /BOI/, gki[2] /GKI_EOF/, gki[3] /LEN_GKIHDR/
include	"gki.com"

begin
	if (!IS_INLINE(fd))
	    call write (gk_fd[fd], gki, GKI_EOF_LEN * SZ_SHORT)
end
