# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mii.h>

# MIIWRITEC -- Write a block of character data to a file in MII format.
# The input data is assumed to be in a machine independent format. 

int procedure miiwritec (fd, spp, nchars)

int	fd			# output file
char	spp[ARB]		# data to be written
int	nchars			# number of chars units to be written

size_t	sz_val
pointer	sp, bp
int	bufsize, status
int	miipksize()

begin
	status = OK
	call smark (sp)

	bufsize = miipksize (nchars, MII_BYTE)
	sz_val = bufsize
	call salloc (bp, sz_val, TY_CHAR)

	call miipak8 (spp, Memc[bp], nchars, TY_CHAR)
	call write (fd, Memc[bp], bufsize)

	call sfree (sp)
	return (status)
end
