# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	"fmio.h"

# FM_LFUNDELETE -- Undelete an lfile.

procedure fm_lfundelete (fm, lfile)

pointer	fm			#I FMIO descriptor
int	lfile			#I lfile number

pointer	lf
errchk	syserrs, fmio_bind, fmio_errchk

begin
	call fmio_bind (fm)
	call fmio_errchk (fm)

        # Verify input.
        if (lfile < 0 || lfile > FM_NLFILES(fm))
            call syserrs (SYS_FMLFNOOB, FM_DFNAME(fm))

        lf = FM_FTABLE(fm) + lfile * LEN_FTE
	LF_FLAGS(lf) = and (LF_FLAGS(lf), not(LFF_DELETED))
	FM_DHMODIFIED(fm) = YES
	call fmio_tick (fm)
end
