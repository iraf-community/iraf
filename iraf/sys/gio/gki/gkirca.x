# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gki.h>

# GKI_RETCELLARRAY -- Return a cell array (pixel array).  Used by a graphics
# kernel to return a cell array to GIO in response to a GETCELLARRAY
# instruction.
#
# BOI GKI_CELLARRAY L NP P
#    
#        L(i)            4 + NP
#        NP(i)           number of pixels in cell array
#        P(NPi)          cell array

procedure gki_retcellarray (fd, m, np)

int	fd			# output file
short	m[ARB]			# cell array
int	np			# number of pixels in cell array

size_t	sz_val
short	gki[GKI_CELLARRAY_LEN]
data	gki[1] /BOI/, gki[2] /GKI_CELLARRAY/

begin
	gki[GKI_CELLARRAY_L] = GKI_CELLARRAY_LEN + np
	gki[GKI_CELLARRAY_NP] = np

	sz_val = GKI_CELLARRAY_LEN * SZ_SHORT
	call write (fd, gki, sz_val)
	sz_val = np * SZ_SHORT
	call write (fd, m, sz_val)
end
