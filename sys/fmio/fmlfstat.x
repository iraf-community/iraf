# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<fmlfstat.h>
include	"fmio.h"

# FMLFSTAT.H -- Query the attributes of an lfile.

int procedure fm_lfstat (fm, lfile, statbuf)

pointer	fm			#I FMIO descriptor
int	lfile			#I lfile number
int	statbuf[ARB]		#O receives status

pointer	lf
errchk	fmio_bind, fmio_errchk

begin
	call fmio_bind (fm)
	call fmio_errchk (fm)

        # Verify input.
        if (lfile < 0 || lfile > FM_NLFILES(fm))
            return (ERR)

	# Copy out the lfile status.
        lf = FM_FTABLE(fm) + lfile * LEN_FTE
	LFU_SIZE(statbuf)  = LF_FSIZE(lf)
	LFU_FLAGS(statbuf) = LF_FLAGS(lf)

	return (OK)
end
