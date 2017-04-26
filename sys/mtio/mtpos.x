# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"mtio.h"

# MTPOSITION -- Position the device to the indicated file and record.
# We are called to position the device by device name, not to position
# an open magtape file.

procedure mtposition (mtname, file, record)

char	mtname[ARB]		#I device to be positioned
int	file			#I desired file number
int	record			#I desired record number

int	junk
pointer	sp, mtspec, device, devcap
errchk	mtparse, mtopen
int	mtopen()

begin
	call smark (sp)
	call salloc (device, SZ_FNAME, TY_CHAR)
	call salloc (mtspec, SZ_FNAME, TY_CHAR)
	call salloc (devcap, SZ_DEVCAP, TY_CHAR)

	# Get device name (including node! prefix) from mtname.
	call mtparse (mtname,
	    Memc[device], SZ_FNAME, junk, junk, Memc[devcap], SZ_DEVCAP)

	# Encode new mtname and open device to position to desired file.
	# Note that we do not return until positioning is complete.  Thus,
	# "mtposition(device,1)" is a rewind with wait.

	call mtencode (Memc[mtspec], SZ_FNAME,
	    Memc[device], file, record, Memc[devcap])
	call close (mtopen (Memc[mtspec], READ_ONLY, 1))

	call sfree (sp)
end
