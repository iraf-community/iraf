# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mii.h>

# MIIWRITEI -- Write a block of integer data to a file in MII format.
# The input data is assumed to be in a machine independent format. 

procedure miiwritei (fd, spp, nints)

int	fd			# output file
int	spp[ARB]		# data to be written
int	nints			# number of int units to be written

pointer	sp, bp
int	bufsize
int	miipksize()

begin
	call smark (sp)

	bufsize = miipksize (nints, MII_LONG)
	call salloc (bp, bufsize, TY_CHAR)

	call miipak32 (spp, Memc[bp], nints, TY_INT)
	call write (fd, Memc[bp], bufsize)

	call sfree (sp)
end
