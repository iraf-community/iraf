# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"mtio.h"

# MTPOSITION -- Position the drive to the indicated file and record.
# We are called to position the drive by device name, not to position an
# open magtape file.

procedure mtposition (filespec, file_number, record_number)

char	filespec[ARB]		# drive to be positioned
int	file_number		# desired file number
int	record_number		# desired record number

int	nchars, junk
pointer	sp, ip, op, mtspec, drive
int	mtopen(), ki_extnode()
errchk	mt_parse, mtopen

begin
	call smark (sp)
	call salloc (drive,  SZ_FNAME, TY_CHAR)
	call salloc (mtspec, SZ_FNAME, TY_CHAR)

	# Get drive name from filespec.
	call mt_parse (filespec, Memc[drive], SZ_FNAME, junk, junk, junk)

	# Encode new filespec and open drive to position to desired file.
	# Note that we do not return until positioning is complete.  Thus,
	# "mtposition(drive,1)" is a rewind with wait.

	ip = drive + ki_extnode (Memc[drive], Memc[mtspec], SZ_FNAME, nchars)
	op = nchars + 1

	call sprintf (Memc[op], SZ_FNAME, "mt%s[%d,%d]")
	    call pargstr (Memc[ip])
	    call pargi (file_number)
	    call pargi (record_number)

	call close (mtopen (Memc[mtspec], READ_ONLY, 1))
	call sfree (sp)
end
