# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mii.h>

# MIIWRITE -- Write a block of data to a file in MII format.
# The input data is in the host system native binary format.

procedure mii_writed (fd, spp, nelem)

int	fd			#I output file
int	spp[ARB]		#I native format data to be written
int	nelem			#I number of data elements to be written

pointer	sp, bp
int	bufsize
int	miipksize()

begin
	call smark (sp)

	bufsize = miipksize (nelem, MII_DOUBLE)
	call salloc (bp, bufsize, TY_CHAR)

	call miipakd (spp, Memc[bp], nelem, TY_DOUBLE)
	call write (fd, Memc[bp], bufsize)

	call sfree (sp)
end
