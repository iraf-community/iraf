# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mii.h>

# MIIWRITE -- Write a block of data to a file in MII format.
# The input data is in the host system native binary format.

int procedure miiwritei (fd, spp, nelem)

int	fd			#I output file
int	spp[ARB]		#I native format data to be written
int	nelem			#I number of data elements to be written

size_t	sz_val
pointer	sp, bp
int	bufsize, status
int	miipksize()

begin
	status = OK
	call smark (sp)

	bufsize = miipksize (nelem, MII_INT)
	sz_val = bufsize
	call salloc (bp, sz_val, TY_CHAR)

	call miipaki (spp, Memc[bp], nelem, TY_INT)
	call write (fd, Memc[bp], bufsize)

	call sfree (sp)
	return (status)
end
