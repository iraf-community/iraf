# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mii.h>

# MIIWRITEC -- Write a block of character data to a file in MII format.
# The input data is assumed to be in a machine independent format. 

procedure mii_writec (fd, spp, nchars)

int	fd			# output file
int	spp[ARB]		# data to be written
int	nchars			# number of chars units to be written

pointer	sp, bp
int	bufsize
int	miipksize()

begin
	call smark (sp)

	bufsize = miipksize (nchars, MII_BYTE)
	call salloc (bp, bufsize, TY_CHAR)

	call miipak8 (spp, Memc[bp], nchars, TY_CHAR)
	call write (fd, Memc[bp], bufsize)

	call sfree (sp)
end
