# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mii.h>

# MIIWRITE -- Write a block of data to a file in MII format.
# The input data is in the host system native binary format.

int procedure miiwrited (fd, spp, nelem)

int	fd			#I output file
double	spp[ARB]		#I native format data to be written
int	nelem			#I number of data elements to be written

size_t	sz_val
pointer	sp, bp
int	bufsize, status
int	miipksize()

begin
	status = OK
	call smark (sp)

	bufsize = miipksize (nelem, MII_DOUBLE)
	sz_val = bufsize
	call salloc (bp, sz_val, TY_CHAR)

	call miipakd (spp, Memc[bp], nelem, TY_DOUBLE)
	call write (fd, Memc[bp], bufsize)

	call sfree (sp)
	return (status)
end
