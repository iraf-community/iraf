# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<error.h>
include	"gkt.h"

.help writeb
.nf ___________________________________________________________________________
WRITEB -- Write an NCAR metacode record.  Always write a full record
regardless of the buffer length; any data beyond buflen is undefined.
If the buffer length is passed as zero, the metafile standard wants us to
write a full (zeroed) record and backspace over it, to signify end of
metafile if the physical metafile is subsequently closed.  Instead of
writing the EOF record here, we leave that to the FIO close routine
for the graphics device.
.endhelp ______________________________________________________________________

procedure writeb (metacode_buffer, buflen, mbunit)

int	metacode_buffer		# LOC pointer to metacode buffer
int	buflen			# number of words of metacode data
int	mbunit			# FIO file descriptor !! from nspp common !!

int	dummy[1], offset
int	loci()
include "gkt.com"

begin
	if (buflen <= 0)
	    return

	# Standard NCAR pointer technique for accessing integer arrays.  This
	# assumes alignment of integer variables.  Convert to use IRAF
	# pointers if this causes problems.

	offset = metacode_buffer - loci (dummy) + 1

        iferr (call write (mbunit, dummy[offset], SZ_MFRECORD))
	    call erract (EA_FATAL)
end
