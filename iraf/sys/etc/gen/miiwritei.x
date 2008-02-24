# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mii.h>

# MIIWRITE -- Write a block of data to a file in MII format.
# The input data is in the host system native binary format.

int procedure miiwritei (fd, spp, nelem)

int	fd			#I output file
int	spp[ARB]		#I native format data to be written
size_t	nelem			#I number of data elements to be written

pointer	sp, bp
size_t	bufsize
int	status
size_t	miipksize()

begin
	status = OK
	call smark (sp)

	bufsize = miipksize (nelem, MII_INT)
	call salloc (bp, bufsize, TY_CHAR)

	call miipaki (spp, Memc[bp], nelem, TY_INT)
	call write (fd, Memc[bp], bufsize)

	call sfree (sp)
	return (status)
end
