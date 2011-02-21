# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <nmi.h>

# NMI_WRITE -- Write a block of data to a file in NMI format.
# The input data is in the host system native binary format.

procedure nmi_writei (fd, spp, nelem)

int	fd			#I output file
int	spp[ARB]		#I native format data to be written
int	nelem			#I number of data elements to be written

pointer	sp, bp
int	bufsize
int	nmipksize()

begin
	call smark (sp)

	bufsize = nmipksize (nelem, NMI_INT)
	call salloc (bp, bufsize, TY_CHAR)

	call nmipaki (spp, Memc[bp], nelem, TY_INT)
	call write (fd, Memc[bp], bufsize)

	call sfree (sp)
end
